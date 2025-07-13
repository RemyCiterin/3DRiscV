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
    { vpn1 :: Bit 10
    , vpn0 :: Bit 10
    , offset :: Bit 12 }
  deriving(Generic, Bits, FShow)

-- Nodes:
--    - A supervisor software can access to a user PTE if SUM is set
data PTE =
  PTE
    { ppn :: Bit 22         -- physical page number
    , rswPTE :: Bit 2       -- reserved for software use
    , dirtyPTE :: Bit 1     -- dirty bit (set at anywrite)
    , accessedPTE :: Bit 1  -- accessed bit (set at any access)
    , globalPTE :: Bit 1    -- don't evitc this entry from TLB at flush
    , userPTE :: Bit 1      -- user can access to this entry
    , executePTE :: Bit 1   -- executable Page Table Entry
    , writePTE :: Bit 1     -- writable Page Table Entry
    , readPTE :: Bit 1      -- readable Page Table Entry
    , validPTE :: Bit 1 }   -- valid Page Table Entry
  deriving(Generic, Bits, Interface)

instance FShow PTE where
  fshow pte =
      fshow "PTE{"
      <> ppn
      <> sep <> valid
      <> sep <> dirty
      <> sep <> accessed
      <> sep <> global
      <> sep <> rsw
      <> sep <> fshow "perms: " <> perms <> noperms
      <> fshow "}"
    where
      sep = fshow ", "
      ppn = fshow "ppn: 0x" <> formatHex 0 (pte.ppn # (0 :: Bit 12))
      rsw = fshow "rsw: " <> fshow pte.rswPTE
      dirty =
        formatCond pte.dirtyPTE (fshow "dirty")
        <> formatCond (inv pte.dirtyPTE) (fshow "clean")
      accessed = fshow "A: " <> fshow pte.accessedPTE
      global = fshow "G: " <> fshow pte.globalPTE
      user = formatCond pte.userPTE (fshow "U")
      execute = formatCond pte.executePTE (fshow "X")
      write = formatCond pte.writePTE (fshow "W")
      read = formatCond pte.readPTE (fshow "R")
      perms = user <> execute <> read <> write
      noperms =
        formatCond
          (inv $ pte.executePTE .||. pte.readPTE .||. pte.writePTE .||. pte.userPTE)
          (fshow "N/A")
      valid =
        formatCond pte.validPTE (fshow "valid")
        <> formatCond (inv pte.validPTE) (fshow "invalid")


isLeafPTE :: PTE -> Bit 1
isLeafPTE pte =
  pte.validPTE .&&. (pte.readPTE .||. pte.writePTE .||. pte.executePTE)

data PtwRequest =
  PtwRequest
    { priv :: Priv
    , virtual :: VirtAddr
    , satp :: Satp
    , atomic :: Bit 1 -- is the access an atomic memory operation
    , store :: Bit 1 -- is the access a store
    , instr :: Bit 1 -- is the access an ifetch
    , mxr :: Bit 1
    , sum :: Bit 1
    , width :: Bit 2 }
  deriving(Generic, Bits, FShow)

isLegalAccess :: PtwRequest -> PTE -> Bit 1
isLegalAccess req pte =
  privLegal .&&. valid .&&. accessLegal
  where
    userLegal = pte.userPTE
    supervisorLegal = pte.userPTE ? (req.sum, true)

    privLegal = req.priv === user_priv ? (userLegal, supervisorLegal)

    valid = pte.validPTE

    accessLegal =
      selectDefault (pte.readPTE .||. (req.mxr .&&. pte.executePTE))
        [ req.store .||. req.atomic --> pte.readPTE .&&. pte.writePTE
        , req.instr --> pte.executePTE ]

-- return an updated PTE after executing a request
-- and a bit to say if a write-back is needed
updatePTE :: PtwRequest -> PTE -> (PTE, Bit 1)
updatePTE req pte =
  ( pte{accessedPTE= true, dirtyPTE= pte.dirtyPTE .||. dirty}
  , inv pte.accessedPTE .||. (inv pte.dirtyPTE .&&. dirty))
    where
      dirty = req.store .||. req.atomic

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
  deriving(Bits, Generic, Interface, FShow)

tlbMatch :: VirtAddr -> TlbEntry -> Bit 1
tlbMatch virt entry = entry.index === 0 ? (match0 .&&. match1, match1)
  where
    match0 = entry.vpn0 === virt.vpn0
    match1 = entry.vpn1 === virt.vpn1

type TlbLogSize = 3

makeDummyPtwFSM :: forall p.
  ( KnownTLParams p
  , 32 ~ AddrWidth p
  , 4 ~ LaneWidth p )
    => (Bit 32 -> Bit 1) -- is a read access at this address possible
    -> (Bit 32 -> Bit 1) -- is a write access at this address possible
    -> (Bit 32 -> Bit 1) -- is an atomic access at this address possible
    -> (Bit 32 -> Bit 1) -- is an instruction access at this address possible
    -> Bit (SourceWidth p)
    -> TLSlave p
    -> Module (Server PtwRequest PtwResponse, Action ())
makeDummyPtwFSM canRead canWrite canAtomic canExec source slave = do
  queue :: Queue PtwRequest <- makePipelineQueue 1
  let virt :: VirtAddr = queue.first.virtual
  let addr :: Bit 32 = pack virt

  let isValid =
        selectDefault (canRead addr)
          [ queue.first.instr  --> canExec addr
          , queue.first.store  --> canWrite addr
          , queue.first.atomic --> canAtomic addr ]

  let reqs = toSink queue

  let aligned =
        select
          [ queue.first.width === 0b00 --> true
          , queue.first.width === 0b01 --> slice @0 @0 addr === 0
          , queue.first.width === 0b10 --> slice @1 @0 addr === 0 ]

  let af_cause =
        selectDefault load_access_fault
          [ queue.first.store  --> store_amo_access_fault
          , queue.first.atomic --> store_amo_access_fault
          , queue.first.instr  --> instruction_access_fault ]

  let align_cause =
        selectDefault load_address_misaligned
          [ queue.first.store  --> store_amo_address_misaligned
          , queue.first.atomic --> store_amo_address_misaligned
          , queue.first.instr  --> instruction_address_misaligned ]

  let tval = pack virt

  let af_output =
        PtwResponse
          { cause= af_cause
          , success= isValid
          , tval= addr
          , rd= addr }

  let align_output =
        PtwResponse
          { cause= align_cause
          , success= aligned
          , tval = addr
          , rd= addr }

  let resps =
        Source
          { canPeek= queue.canDeq
          , peek=
              aligned ? (af_output, align_output)
          , consume= queue.deq}

  return (Server{reqs,resps}, pure ())

makePtwFSM :: forall p.
  ( KnownTLParams p
  , 32 ~ AddrWidth p
  , 4 ~ LaneWidth p )
    => (Bit 32 -> Bit 1) -- is a read access at this address possible
    -> (Bit 32 -> Bit 1) -- is a write access at this address possible
    -> (Bit 32 -> Bit 1) -- is an atomic access at this address possible
    -> (Bit 32 -> Bit 1) -- is an instruction access at this address possible
    -> Bit (SourceWidth p)
    -> TLSlave p
    -> Module (Server PtwRequest PtwResponse, Action ())
makePtwFSM canRead canWrite canAtomic canExec source slave = do
  outputs :: Queue PtwResponse <- makeBypassQueue

  inputs :: Queue PtwRequest <- makePipelineQueue 1
  let virt = inputs.first.virtual
  let satp = inputs.first.satp
  let priv = inputs.first.priv

  let isValid :: Bit 32 -> Bit 1 = \ addr ->
        selectDefault (canRead addr)
          [ inputs.first.instr  --> canExec addr
          , inputs.first.store  --> canWrite addr
          , inputs.first.atomic --> canAtomic addr ]

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
          [ inputs.first.width === 0b00 --> true
          , inputs.first.width === 0b01 --> slice @0 @0 virt.offset === 0
          , inputs.first.width === 0b10 --> slice @1 @0 virt.offset === 0 ]

  let pf_cause =
        selectDefault load_page_fault
          [ inputs.first.store  --> store_amo_page_fault
          , inputs.first.atomic --> store_amo_page_fault
          , inputs.first.instr  --> instruction_page_fault ]

  let af_cause =
        selectDefault load_access_fault
          [ inputs.first.store  --> store_amo_access_fault
          , inputs.first.atomic --> store_amo_access_fault
          , inputs.first.instr  --> instruction_access_fault ]

  let align_cause =
        selectDefault load_address_misaligned
          [ inputs.first.store  --> store_amo_address_misaligned
          , inputs.first.atomic --> store_amo_address_misaligned
          , inputs.first.instr  --> instruction_address_misaligned ]

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

  index :: Reg (Bit 1) <- makeReg dontCare
  addr :: Reg (Bit 34) <- makeReg dontCare

  -- Don't Perform Address Translation (early output due to a fault)
  abort :: Reg (Bit 1) <- makeReg false

  -- Delay `inputs.deq` of one cycle to ensure we don't write two times
  -- in abort or stop in the same cycle (inputs is a pipeline fifo)
  deqInput :: Reg (Bit 1) <- makeDReg false
  always $ when deqInput.val inputs.deq

  -- Finish the `Page Table Walk` procedure
  stop :: Reg (Bit 1) <- makeReg false

  -- Used by `Page Table Walk` and `Tlb Lookup` to return their results,
  -- because the results in `tlb` can be corrupted because of a flush
  match :: Reg TlbEntry <- makeReg dontCare

  let align_fail :: Action () = do
        outputs.enq align_output{success=false}
        deqInput <== true
  let af_fail :: Action () = do
        outputs.enq af_output{success=false}
        deqInput <== true
  let pf_fail :: Action () = do
        outputs.enq pf_output{success=false}
        deqInput <== true

  -- we write in port 0 of idle:
  --     do not write in any register during a call to succede,
  --     otherwise we may double write because of a new request
  let succede :: Bit 32 -> Action () = \ rd -> do
        outputs.enq (pf_output{rd} :: PtwResponse)
        inputs.deq

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
  hasFlush :: Reg (Bit 1) <- makeReg false

  -- Warning: it must be statically clear that each loop step take at
  -- least one cycle, otherwise we will observe a circular path
  withName "runStmt" $ runStmt do
    while true do
      wait (inputs.canDeq .&&. outputs.notFull .&&. inv deqInput.val)

      if inv aligned then do
        action align_fail

      else if priv === machine_priv .||. inv satp.useSv32 then do
        let addr :: Bit 32 = pack virt

        if isValid addr then do
          action (succede addr)
        else do
          action af_fail

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
                  when (inv hasFlush.val) do
                    tlb!way.val <== some out
                  match <== out
                  stop <== true
                else if inv pte.validPTE .||. index.val === 0 then do
                  -- Address translation failure
                  display "Invalid address translation with:\n\t" pte "\n\tat " (formatHex 0 lsb)
                  display "\t" slave.channelD.peek
                  abort <== true
                  stop <== true
                  pf_fail
                else do
                  -- Next stage
                  index <== index.val - 1
                  addr <== pte.ppn # virt.vpn0 # (0b00 :: Bit 2)

        if inv abort.val then do
          -- Translation Step
          let entry = match.val
          let pte = entry.pte
          let addr :: Bit 34 =
                entry.index === 0 ?
                  (pte.ppn # virt.offset, upper pte.ppn # virt.vpn0 # virt.offset)
          if slice @33 @32 addr =!= 0 .||. inv (isValid (lower addr)) then do
            action do
              af_fail

          else if isLegalAccess inputs.first pte then do
            let (newPte, diff) = updatePTE inputs.first pte

            when diff do
              wait slave.channelA.canPut
              action (put entry.address (pack newPte))
              wait (inv doFlush.val)
              action (tlb!way.val <== some entry{pte=newPte})
              wait slave.channelD.canPeek
              action slave.channelD.consume

            action (succede (lower addr))

          else do
            action pf_fail
        else do
          tick

  let flush = do
        doFlush <== true
        hasFlush <== true
        forM_ tlb \ r -> do
          r <== none

  return
    ( Server
        { reqs=
            Sink
              { canPut= inputs.notFull .&&. inv doFlush.val
              , put= \ x -> do
                  -- Tlb Lookup
                  let (w,h) = tlbSearch x.virtual
                  addr <== x.satp.ppn # x.virtual.vpn1 # (0b00 :: Bit 2)
                  match <== (tlb!w).val.val
                  hasFlush <== false
                  abort <== false
                  stop <== false
                  inputs.enq x
                  index <== 1
                  way <== w
                  hit <== h }
        , resps= toSource outputs}
    , flush )
