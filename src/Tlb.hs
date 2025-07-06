module Tlb where

import Instr
import Blarney
import Blarney.ADT
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
  pte.validPTE .&&. (pte.readPTE .||. pte.writePTE .||. pte.executePTE)

data PtwRequest =
  PtwRequest
    { priv :: Priv
    , virtual :: VirtAddr
    , satp :: Satp
    , store :: Bit 1 -- is the access a store
    , instr :: Bit 1 -- is the access an ifetch
    , mxr :: Bit 1
    , sum :: Bit 1
    , width :: Bit 2 }
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
data PtwResponse =
  PtwResponse
    { success :: Bit 1
    , cause :: CauseException
    , tval :: Bit 32
    , rd :: Bit 32 }
    deriving(Generic, Bits)

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

type TlbLogSize = 3

makePtwFSM :: forall p.
  ( KnownTLParams p
  , 32 ~ AddrWidth p
  , 4 ~ LaneWidth p )
    => Bit (SourceWidth p)
    -> Source PtwRequest
    -> TLSlave p
    -> Module (Server PtwRequest PtwResponse, Action ())
makePtwFSM source inputs slave = do
  outputs :: Queue PtwResponse <- makeBypassQueue
  flushAck :: Queue () <- makeQueue

  input :: Reg PtwRequest <- makeReg dontCare
  let virt = input.val.virtual
  let satp = input.val.satp
  let priv = input.val.priv

  -- TLB is keek small for now
  tlb :: [Reg (Option TlbEntry)] <- replicateM (valueOf @(2^TlbLogSize)) (makeReg none)
  randomWay :: Reg (Bit TlbLogSize) <- makeReg 0
  always (randomWay <== randomWay.val + 1)

  way :: Reg (Bit TlbLogSize) <- makeReg dontCare
  hit :: Reg (Bit 1) <- makeReg dontCare

  let tlbSearch :: VirtAddr -> (Bit TlbLogSize, Bit 1) = \ x ->
        selectDefault (randomWay.val, false)
          [ r.val.valid .&&. tlbMatch x r.val.val --> (lit i, true)
            | (i, r) <- zip [0..] tlb ]

  let aligned =
        select
          [ input.val.width === 0b00 --> true
          , input.val.width === 0b01 --> slice @0 @0 virt.offset === 0
          , input.val.width === 0b10 --> slice @1 @0 virt.offset === 0 ]

  let pf_cause =
        selectDefault load_page_fault
          [ input.val.store --> store_amo_page_fault
          , input.val.instr --> instruction_page_fault ]

  let af_cause =
        selectDefault load_access_fault
          [ input.val.store --> store_amo_access_fault
          , input.val.instr --> instruction_access_fault ]

  let align_cause =
        selectDefault load_address_misaligned
          [ input.val.store --> store_amo_address_misaligned
          , input.val.instr --> instruction_address_misaligned ]

  let tval = pack virt

  let pf_output =
        PtwResponse
          { cause= pf_cause
          , success= true
          , rd= pack virt
          , tval }

  let af_output =
        PtwResponse
          { cause= af_cause
          , success= true
          , rd= pack virt
          , tval }

  let align_output =
        PtwResponse
          { cause= align_cause
          , success= true
          , rd= pack virt
          , tval }

  idle :: Ehr (Bit 1) <- makeEhr 2 true
  index :: Reg (Bit 1) <- makeReg dontCare
  addr :: Reg (Bit 34) <- makeReg dontCare
  abort :: Reg (Bit 1) <- makeReg false

  -- Stop the `Page Table Walk` procedure
  stop :: Reg (Bit 1) <- makeReg false

  -- Used by `Page Table Walk` and `Tlb Lookup` to return their results,
  -- because the results in `tlb` can be corrupted because of a flush
  match :: Reg TlbEntry <- makeReg dontCare

  let align_fail :: Action () = do
        outputs.enq align_output{success=false}
        idle.write 1 true
  let af_fail :: Action () = do
        outputs.enq af_output{success=false}
        idle.write 1 true
  let pf_fail :: Action () = do
        outputs.enq pf_output{success=false}
        idle.write 1 true

  -- we write in port 0 of idle:
  --     do not write in any register during a call to succede,
  --     otherwise we may double write because of a new request
  let succede :: Bit 32 -> Action () = \ rd -> do
        outputs.enq (pf_output{rd} :: PtwResponse)
        idle.write 0 true

  let get address =
        slave.channelA.put
          ChannelA
            { opcode= item #Get
            , lane= dontCare
            , mask= ones
            , address
            , size= 2
            , source }

  let put address lane =
        slave.channelA.put
          ChannelA
            { opcode= item #PutData
            , mask= ones
            , address
            , size= 2
            , source
            , lane }

  doFlush :: Wire (Bit 1) <- makeWire false

  runStmt do
    while true do
      wait (inv (idle.read 0) .&&. outputs.notFull)

      if inv aligned then do
        action align_fail

      else if priv === machine_priv .||. inv satp.useSv32 then do
        action pf_fail

      else do
        when (inv hit.val) do
          -- Page Table Walk
          while (inv stop.val) do
            let (msb, lsb) = split addr.val
            if msb =!= 0 then do
              action do
                stop <== true
                abort <== true
                af_fail
            else do
              wait slave.channelA.canPut
              action (get lsb)
              wait (slave.channelD.canPeek .&&. inv doFlush.val)
              let pte = unpack slave.channelD.peek.lane
              action do
                slave.channelD.consume
                if isLeafPTE pte then do
                  let out =
                        TlbEntry
                          { address= lsb
                          , index= index.val
                          , vpn0= virt.vpn0
                          , vpn1= virt.vpn1
                          , pte }
                  tlb!way.val <== some out
                  match <== out
                  stop <== true
                else if inv pte.validPTE .||. index.val === 0 then do
                  -- Address translation failure
                  abort <== true
                  stop <== true
                  pf_fail
                else do
                  -- Next stage
                  index <== index.val - 1
                  addr <== pte.ppn # virt.vpn0 # (0b00 :: Bit 2)

        when (inv abort.val) do
          let entry = match.val
          let pte = entry.pte
          if isLegalAccess input.val entry.pte then do
            let (newPte, diff) = updatePTE input.val pte

            when diff do
              wait slave.channelA.canPut
              action (put entry.address (pack newPte))
              action (tlb!way.val <== some entry{pte=newPte})
              wait slave.channelD.canPeek
              action slave.channelD.consume

            wait (inv doFlush.val)
            action do
              let addr :: Bit 34 =
                    entry.index === 0 ?
                      (pte.ppn # virt.offset, upper pte.ppn # virt.vpn0 # virt.offset)
              if slice @33 @32 addr === 0 then
                succede (lower addr)
              else
                af_fail
          else do
            action pf_fail

  let flush = do
        doFlush <== true
        forM_ tlb \ r -> do
          r <== none

  return
    ( Server
        { reqs=
            Sink
              { canPut= idle.read 1
              , put= \ x -> do
                  let (w,h) = tlbSearch x.virtual
                  addr <== satp.ppn # x.virtual.vpn1 # (0b00 :: Bit 2)
                  match <== (tlb!w).val.val
                  idle.write 1 false
                  abort <== false
                  stop <== false
                  input <== x
                  index <== 1
                  way <== w
                  hit <== h }
        , resps= toSource outputs}
    , flush )
