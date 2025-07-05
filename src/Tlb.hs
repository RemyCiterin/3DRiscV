module Tlb where

import Instr
import Blarney
import Blarney.Ehr
import Blarney.Utils
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Stmt


import TileLink
import Instr
import CSR

data VirtAddr =
  VirtAddr
    { offset :: Bit 12
    , vpn0 :: Bit 10
    , vpn1 :: Bit 10 }
  deriving(Generic, Bits)

data Satp =
  Satp
    { useSv32 :: Bit 1
    , asid :: Bit 9
    , ppn :: Bit 22 }
  deriving(Bits, Generic, Interface, FShow)

-- Nodes:
--    - A supervisor software can access to a user PTE if SUM is set
data PTE =
  PTE
    { validPTE :: Bit 1     -- valid Page Table Entry
    , readPTE :: Bit 1      -- readable Page Table Entry
    , writePTE :: Bit 1     -- writable Page Table Entry
    , executePTE :: Bit 1   -- executable Page Table Entry
    , userPTE :: Bit 1      -- user can access to this entry
    , globalPTE :: Bit 1    -- don't evitc this entry from TLB at flush
    , accessedPTE :: Bit 1  -- accessed bit (set at any access)
    , dirtyPTE :: Bit 1     -- dirty bit (set at anywrite)
    , rswPTE :: Bit 2       -- reserved for software use
    , ppn :: Bit 22 }       -- physical page number
  deriving(Generic, Bits, FShow, Interface)

isLeafPTE :: PTE -> Bit 1
isLeafPTE pte =
  pte.validPTE .&&. inv (pte.readPTE .||. pte.writePTE .||. pte.writePTE)

data PtwRequest =
  PtwRequest
    { priv :: Priv
    , virtual :: VirtAddr
    , satp :: Satp
    , store :: Bit 1 -- is the access a store
    , instr :: Bit 1 -- is the access an ifetch
    , mxr :: Bit 1
    , sum :: Bit 1 }
  deriving(Generic, Bits)

isLegalAccess :: PtwRequest -> PTE -> Bit 1
isLegalAccess req pte =
  privLegal .&&. valid .&&. accessLegal
  where
    userLegal = pte.userPTE
    supervisorLegal = pte.userPTE ? (req.sum, false)

    privLegal = req.priv === user_priv ? (userLegal, supervisorLegal)

    valid = pte.validPTE

    accessLegal =
      selectDefault (pte.readPTE .||. (req.mxr .&&. pte.executePTE))
        [ req.store --> pte.readPTE .&&. pte.writePTE -- illegal PTE otherwise
        , req.instr --> pte.executePTE ]

-- return an updated PTE after executing a request
-- and a bit to say if a write-back is needed
updatePTE :: PtwRequest -> PTE -> (PTE, Bit 1)
updatePTE req pte =
  ( pte{accessedPTE= true, dirtyPTE= pte.dirtyPTE .||. req.store}
  , inv pte.accessedPTE .||. (inv pte.dirtyPTE .&&. req.store))

-- If output.exception:
--  - return the execption
-- Otherwise:
--  - rd constains the translated address
type PtwResponse = ExecOutput

data TlbEntry =
  TlbEntry
    { vpn0 :: Bit 10
    , vpn1 :: Bit 10
    , index :: Bit 1
    , address :: Bit 32
    , pte :: PTE }
  deriving(Bits, Generic, Interface)

tlbMatch :: VirtAddr -> TlbEntry -> Bit 1
tlbMatch virt entry = entry.index === 0 ? (match0 .&&. match1, match1)
  where
    match0 = entry.vpn0 === virt.vpn0
    match1 = entry.vpn1 === virt.vpn1

makePtwFSM :: forall p.
  ( KnownTLParams p
  , 32 ~ AddrWidth p
  , 4 ~ LaneWidth p )
    => Bit (SourceWidth p)
    -> Source PtwRequest
    -> TLSlave p
    -> Module (Server PtwRequest PtwResponse)
makePtwFSM source inputs slave = do
  outputs :: Queue PtwResponse <- makeBypassQueue

  input :: Reg PtwRequest <- makeReg dontCare

  -- TLB is keek small for now
  tlb :: [Reg (Option TlbEntry)] <- replicateM 8 (makeReg none)
  counter :: Reg (Bit 3) <- makeReg 0
  always (counter <== counter.val + 1)
  way :: Reg (Bit 3) <- makeReg dontCare
  hit :: Reg (Bit 1) <- makeReg dontCare

  let tlbSearch :: VirtAddr -> (Bit 3, Bit 1) = \ x ->
        selectDefault (counter.val, false)
          [ r.val.valid .&&. tlbMatch x r.val.val --> (lit i, true)
            | (i, r) <- zip [0..] tlb ]

  idle :: Ehr (Bit 1) <- makeEhr 2 true
  index :: Reg (Bit 2) <- makeReg dontCare
  addr :: Reg (Bit 32) <- makeReg dontCare
  pte :: Reg PTE <- makeReg dontCare
  needWB :: Reg (Bit 1) <- makeReg false


  runStmt do
    while true do
      wait (inv (idle.read 0) .&&. slave.channelA.canPut .&&. outputs.notFull)
      action do
        pure ()

  return
    Server
      { reqs=
          Sink
            { canPut= idle.read 1
            , put= \ x -> do
                let (w,h) = tlbSearch x.virtual
                idle.write 1 false
                input <== x
                way <== w
                hit <== h }
      , resps= (toSource outputs){canPeek= outputs.canDeq .&&. inv needWB.val}}
