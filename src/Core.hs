module Core where

import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Connectable
import Blarney.Utils
import Blarney.ADT

import Prediction
import System
import Instr
import Clint
import Uart
import Spi
import Alu
import CSR
import MMU

import TileLink
import TileLink.CoherentBCache
import TileLink.Broadcast

displayAscii :: Bit 8 -> Action ()
displayAscii term =
  pure ()
  --display_ $ go term $ 10 : 13 : [32..126]
  --where
  --  ascii :: [String] = [[toEnum i] | i <- [0..255]]

  --  go :: Bit 8 -> [Integer] -> Format
  --  go x [] = formatCond false (fshow "")
  --  go x (i : is)  =
  --    formatCond (fromInteger i === x) (fshow (ascii!i)) <>
  --    go x is

debug :: Bool
debug = False

isCached :: Bit 32 -> Bit 1
isCached addr = addr .>=. 0x80000000

type EpochWidth = 8
type Epoch = Bit EpochWidth

type RasSize = 3
type HistSize = 8

data FetchOutput = FetchOutput {
    bstate :: BPredState HistSize RasSize,
    exception :: Bit 1,
    cause :: CauseException,
    prediction :: Bit 32,
    instr :: Bit 32,
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

data DecodeOutput = DecodeOutput {
    bstate :: BPredState HistSize RasSize,
    prediction :: Bit 32,
    exception :: Bit 1,
    cause :: CauseException,
    instr :: Instr,
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

data Redirection = Redirection {
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

type Training =
  BPredState HistSize RasSize -> Bit 32 -> Bit 32 -> Option Instr -> Action ()

-- 32 bit of address
-- 4 bytes of data
-- 4 bits of size configuration
-- 8 bits of source identifier
-- 8 ibts of sink identifier
type TLConfig = TLParams 32 4 4 8 8
type TLConfig' = TLParams 32 4 4 8 0

makeICache ::
  VMInfo
  -> TLSlave TLConfig -- cache slave
  -> TLSlave TLConfig -- mmu slave
  -> Bit (SourceWidth TLConfig) -- cache source
  -> Bit (SourceWidth TLConfig) -- mmu source
  -> Module (Server (Bit 32) MmuResponse, Action ())
makeICache vminfo cacheSlave mmuSlave cacheSource mmuSource = do
  cache <-
    withName "icache" $
      makeBCacheCoreWith @2 @20 @6 @4 @_ @TLConfig cacheSource cacheSlave execAMO

  (Server{reqs= mmuIn, resps= mmuOut}, tlbFlush) <- withName "ITLB" $
    makeMmuFSM (\_ -> true) (\ _ -> true) isCached isCached mmuSource mmuSlave

  tagQ :: Queue MmuResponse <- makePipelineQueue 2

  key :: Reg (Bit 20) <- makeReg dontCare

  withName "icache" $ always do
    when (cache.canMatch) do
      cache.match key.val

    when (cache.canLookup .&&. mmuOut.canPeek .&&. tagQ.notFull) do
      when (mmuOut.peek.success) do
        let phys = mmuOut.peek.rd
        cache.lookup (slice @11 @6 phys) (slice @5 @2 phys) (item #Load)
        key <== slice @31 @12 phys
      tagQ.enq mmuOut.peek
      mmuOut.consume

  let reqs =
        Sink
          { canPut= mmuIn.canPut
          , put= \ x -> do
              mmuIn.put
                MmuRequest
                  { virtual= unpack x
                  , satp= vminfo.satp
                  , priv= vminfo.priv
                  , sum= vminfo.sum
                  , mxr= vminfo.mxr
                  , atomic= false
                  , store= false
                  , instr= true
                  , width= 0b10 }}

  -- Ensure that the instruction will be executed by the Alu in case
  -- of a misaligned instruction exception
  let rd = tagQ.first.success ? (cache.loadResponse.peek, 0)
  let resps=
        Source
          { canPeek= tagQ.canDeq .&&. (inv tagQ.first.success .||. cache.loadResponse.canPeek)
          , peek= tagQ.first{rd} :: MmuResponse
          , consume= do
              when tagQ.first.success do
                cache.loadResponse.consume
              tagQ.deq }

  return (Server{reqs, resps}, tlbFlush)

makeFetch ::
  VMInfo ->
  Bit (SourceWidth TLConfig) ->
  Bit (SourceWidth TLConfig) ->
  Stream Redirection ->
  Module (TLMaster TLConfig, Stream FetchOutput, Training, Training, Action ())
makeFetch vminfo fetchSource mmuSource redirection = do
  bpred :: BranchPredictor HistSize RasSize EpochWidth <-
    withName "bpred" $ makeBranchPredictor 10

  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ _ -> 0
          , rootSink= \ _ -> 0
          , rootSource= \ x ->
              select
                [ x === fetchSource --> 0
                , x === mmuSource --> 1 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master], [cacheSlave,mmuSlave]) <- makeTLXBar @1 @2 @TLConfig xbarconfig

  (cache, flush) <- makeICache vminfo cacheSlave mmuSlave fetchSource mmuSource

  pc :: Ehr (Bit 32) <- makeEhr 2 0x80000000
  epoch :: Ehr Epoch <- makeEhr 2 0

  queue :: Queue () <- makePipelineQueue 1

  fetchQ :: Queue (Option (Bit 32, Bit 32, BPredState HistSize RasSize, Epoch)) <-
    makeSizedQueueCore 4

  outputQ :: Queue FetchOutput <- makeQueue

  responseQ :: Queue MmuResponse <- makeSizedQueueCore 8

  always do
    when (cache.resps.canPeek .&&. responseQ.notFull) do
      responseQ.enq cache.resps.peek
      cache.resps.consume

    when (cache.reqs.canPut .&&. queue.notFull) do
      bpred.start (pc.read 1) (epoch.read 1)
      cache.reqs.put (pc.read 1)
      queue.enq ()

    when (queue.canDeq .&&. fetchQ.notFull .&&. inv redirection.canPeek) do
      if epoch.read 0 === bpred.request.snd then do
        (predPc, state) <- bpred.predict

        fetchQ.enq $ some (pc.read 0, predPc, state, epoch.read 0)
        pc.write 0 predPc
      else do
        fetchQ.enq none

      queue.deq

    when (fetchQ.canDeq .&&. responseQ.canDeq .&&. outputQ.notFull) do
      when fetchQ.first.valid do
        let (outPc, outPred, outState, outEpoch) = fetchQ.first.val

        outputQ.enq FetchOutput{
          exception= inv responseQ.first.success,
          cause= responseQ.first.cause,
          instr= responseQ.first.rd,
          prediction= outPred,
          bstate= outState,
          epoch= outEpoch,
          pc= outPc
        }

      responseQ.deq
      fetchQ.deq

    when redirection.canPeek do
      pc.write 1 redirection.peek.pc
      epoch.write 1 redirection.peek.epoch
      redirection.consume

  return (master, toStream outputQ, bpred.trainHit, bpred.trainMis, flush)

makeDecode :: Stream FetchOutput -> Module (Stream DecodeOutput)
makeDecode stream = do
  outputQ :: Queue DecodeOutput <- makeQueue

  always do
    when (stream.canPeek .&&. outputQ.notFull) do
      let req = stream.peek
      outputQ.enq DecodeOutput
        { instr= decodeInstr req.instr
        , exception= req.exception
        , cause= req.cause
        , prediction= req.prediction
        , bstate= req.bstate
        , epoch= req.epoch
        , pc=req.pc }
      stream.consume

  return (toStream outputQ)

data DMmuResponse =
  DMmuResponse
    { exception :: Bit 1
    , cause :: CauseException
    , tval :: Bit 32
    , virt :: Bit 32
    , instr :: Instr
    , phys :: Bit 32
    , lane :: Bit 32
    , pc :: Bit 32 }
    deriving(Generic, Bits)

makeAGU ::
  VMInfo ->
  Bit (SourceWidth TLConfig) ->
  TLSlave TLConfig ->
  Stream ExecInput ->
  Module (Stream DMmuResponse, Action ())
makeAGU vminfo mmuSource mmuSlave inputs = do
  (Server{reqs= mmuIn, resps= mmuOut}, tlbFlush) <- withName "DTLB" $
    makeMmuFSM (\_ -> true) (\ _ -> true) isCached isCached mmuSource mmuSlave

  state :: Queue ExecInput <- makePipelineQueue 1

  always do
    when (inputs.canPeek .&&. mmuIn.canPut .&&. state.notFull) do
      let instr = inputs.peek.instr

      let virt = inputs.peek.rs1 + instr.imm.val
      let width =
            (instr.opcode `is` [LOAD,STORE]) ?
              (instr.accessWidth, 0b10)
      when (inv (instr.opcode `is` [FENCE])) do
        mmuIn.put
          MmuRequest
            { atomic= instr.isAMO .||. instr.opcode `is` [STOREC,LOADR]
            , store= instr.opcode `is` [STORE]
            , virtual= unpack virt
            , satp= vminfo.satp
            , priv= vminfo.priv
            , mxr= vminfo.mxr
            , sum= vminfo.sum
            , instr= false
            , width }
      state.enq inputs.peek
      inputs.consume

  let isFence = state.first.instr.opcode `is` [FENCE]
  return
    ( Source
      { canPeek= (mmuOut.canPeek .||. isFence) .&&. state.canDeq
      , consume= do
          when (inv isFence) mmuOut.consume
          state.deq
      , peek=
        DMmuResponse
          { exception= inv (mmuOut.peek.success .||. isFence)
          , cause= mmuOut.peek.cause
          , tval= mmuOut.peek.tval
          , virt= state.first.rs1 + state.first.instr.imm.val
          , instr= state.first.instr
          , phys= mmuOut.peek.rd
          , lane= state.first.rs2
          , pc= state.first.pc }}
    , tlbFlush )

data DMemRequest =
  DMemRequest
    { addr :: Bit 32
    , width :: Bit 2 -- width of the memory operation
    , load :: Bit 1 -- load or load-reserve
    , store :: Bit 1 -- store or store-conditional
    , unique :: Bit 1 -- LR/SC operation
    , isUnsigned :: Bit 1 -- is the load/loadr signed
    , amo :: Option (MnemonicVec, Bit 32)
    , lane :: Bit 32 }
  deriving(Bits, Generic)

makeDMem ::
  Bit (SourceWidth TLConfig) ->
  Bit (SourceWidth TLConfig) ->
  TLSlave TLConfig ->
  TLSlave TLConfig ->
  Module (Sink DMemRequest, Source (Bit 32))
makeDMem cacheSource mmioSource cacheSlave mmioSlave = do
  key :: Reg (Bit 32) <- makeReg dontCare

  queueA :: Queue (ChannelA TLConfig) <- makeQueue
  queueD :: Queue (ChannelD TLConfig) <- makeQueue
  makeConnection (toSource queueA) mmioSlave.channelA
  makeConnection mmioSlave.channelD (toSink queueD)

  cache <-
    withName "dcache" $
      makeBCacheCoreWith @2 @20 @6 @4 @_ @TLConfig cacheSource cacheSlave execAMO

  always do
    when (cache.canMatch) do
      cache.match (upper key.val)

  inflight_uncached_memop :: Reg (Bit 1) <- makeReg false

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  metaQ :: Queue (Bit 2, Bit 2, Bit 1) <- makePipelineQueue 2
  tagQ :: Queue (Bit 8) <- makePipelineQueue 2
  let cached_load_tag :: Bit 8 = 0
  let cached_store_tag :: Bit 8 = 1
  let uncached_memop_tag :: Bit 8 = 2
  let cached_amo_tag :: Bit 8 = 3
  let cached_loadr_tag :: Bit 8 = 4
  let cached_storec_tag :: Bit 8 = 5

  return
    ( Sink
      { canPut=
          inv inflight_uncached_memop.val .&&.
          cache.canLookup .&&.
          metaQ.notFull .&&.
          tagQ.notFull .&&.
          queueA.notFull
      , put= \ req -> do
          key <== req.addr
          let lookup = \ x -> cache.lookup (slice @11 @6 req.addr) (slice @5 @2 req.addr) x
          let width = (req.load .||. req.store) ? (req.width, 0b10)

          metaQ.enq
            (slice @1 @0 req.addr, width, req.isUnsigned)

          let isWord = width === 0b10
          let isHalf = width === 0b01
          let isByte = width === 0b00

          let lane = req.lane .<<. ((slice @1 @0 req.addr) # (0b000 :: Bit 3))
          let mask :: Bit 4 =
                select [
                  isByte --> 0b0001,
                  isHalf --> 0b0011,
                  isWord --> 0b1111
                ] .<<. (slice @1 @0 req.addr)

          if (inv (isCached req.addr)) then do
            inflight_uncached_memop <== true
            when (req.store .&&. req.addr === 0x10000000) $ displayAscii (slice @7 @0 lane)
            tagQ.enq uncached_memop_tag
            queueA.enq
              ChannelA
                { opcode= req.store ? (item #PutData, item #Get)
                , size= zeroExtend width
                , source= mmioSource
                , address= req.addr
                , lane
                , mask }

          else do
            when (req.store .&&. inv req.unique) do
              lookup (tag #Store (mask,lane))
              tagQ.enq cached_store_tag
            when (req.store .&&. req.unique) do
              lookup (tag #StoreC (mask,lane))
              tagQ.enq cached_storec_tag
            when (req.load .&&. inv req.unique) do
              lookup (item #Load)
              tagQ.enq cached_load_tag
            when req.amo.valid do
              lookup (tag #Atomic req.amo.val)
              tagQ.enq cached_amo_tag
            when (req.load .&&. req.unique) do
              lookup (item #LoadR)
              tagQ.enq cached_loadr_tag }
    , Source
      { canPeek=
          tagQ.canDeq .&&. metaQ.canDeq .&&.
            (
              (tagQ.first === uncached_memop_tag .&&. queueD.canDeq) .||.
              (tagQ.first === cached_amo_tag .&&. cache.atomicResponse.canPeek) .||.
              (tagQ.first === cached_loadr_tag .&&. cache.loadResponse.canPeek) .||.
              (tagQ.first === cached_load_tag .&&. cache.loadResponse.canPeek) .||.
              (tagQ.first === cached_storec_tag .&&. cache.scResponse.canPeek) .||.
              tagQ.first === cached_store_tag
            )
      , peek=
          let (offset, width, isUnsigned) = metaQ.first in
          let isWord = width === 0b10 in
          let isHalf = width === 0b01 in
          let isByte = width === 0b00 in
          let lane =
                select
                  [ tagQ.first === uncached_memop_tag --> queueD.first.lane
                  , tagQ.first === cached_amo_tag --> cache.atomicResponse.peek
                  , tagQ.first === cached_loadr_tag --> cache.loadResponse.peek
                  , tagQ.first === cached_load_tag --> cache.loadResponse.peek
                  , tagQ.first === cached_storec_tag --> zeroExtend (inv cache.scResponse.peek)
                  , tagQ.first === cached_store_tag --> dontCare
                  ] .>>. (offset # (0 :: Bit 3)) in

          select
            [ isByte .&&. isUnsigned --> zeroExtend (slice @7 @0 lane)
            , isHalf .&&. isUnsigned --> zeroExtend (slice @15 @0 lane)
            , isByte .&&. inv isUnsigned --> signExtend (slice @7 @0 lane)
            , isHalf .&&. inv isUnsigned --> signExtend (slice @15 @0 lane)
            , isWord --> lane ]
      , consume= do
          when (tagQ.first === cached_amo_tag) cache.atomicResponse.consume
          when (tagQ.first === cached_loadr_tag) cache.loadResponse.consume
          when (tagQ.first === cached_load_tag) cache.loadResponse.consume
          when (tagQ.first === cached_storec_tag) cache.scResponse.consume
          when (tagQ.first === uncached_memop_tag) do
            inflight_uncached_memop <== false
            queueD.deq
          metaQ.deq
          tagQ.deq
      })

makeLoadStoreUnit ::
  VMInfo ->
  Bit (SourceWidth TLConfig) ->
  Bit (SourceWidth TLConfig) ->
  Bit (SourceWidth TLConfig) ->
  Module (TLSlave TLConfig, TLMaster TLConfig, Server DMmuResponse (RegId, Bit 32))
makeLoadStoreUnit vminfo cacheSource mmuSource mmioSource = do
  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ _ -> 0
          , rootSink= \ _ -> 0
          , rootSource= \ x ->
              select
                [ x === cacheSource --> 0
                , x === mmioSource --> 1
                , x === mmuSource --> 2 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master], [cacheSlave,mmioSlave,mmuSlave,_]) <- makeTLXBar @1 @4 @TLConfig xbarconfig

  outputQ :: Queue (RegId, Bit 32) <- makeQueue

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  (memIn, memOut) <- withName "dmem" $ makeDMem cacheSource mmioSource cacheSlave mmioSlave

  regQ :: Queue (Bit 1, RegId) <- makePipelineQueue 2

  let sink =
        Sink
          { canPut= memIn.canPut .&&. regQ.notFull
          , put= \ req -> do
              let instr = req.instr
              let opcode = instr.opcode
              let virt = req.virt
              let phys = req.phys

              let isFence = opcode `is` [FENCE]

              let request =
                    DMemRequest
                      { width= instr.accessWidth
                      , isUnsigned= instr.isUnsigned
                      , amo= instr.isAMO ? (some (opcode, req.lane), none)
                      , unique= opcode `is` [STOREC,LOADR]
                      , store= opcode `is` [STORE,STOREC]
                      , load= opcode `is` [LOAD,LOADR]
                      , lane= req.lane
                      , addr= phys }

              regQ.enq (isFence, instr.rd.valid ? (instr.rd.val, 0))
              when (inv isFence) $ memIn.put request }

  always do
    when (regQ.canDeq .&&. outputQ.notFull) do
      let (isFence, rd) = regQ.first

      when (isFence .||. memOut.canPeek) do
        when (inv isFence) memOut.consume
        outputQ.enq (rd, memOut.peek)
        regQ.deq

  return (mmuSlave, master, Server{reqs= sink, resps= toSource outputQ})

data RegisterFile =
  RegisterFile
    { setReady :: RegId -> Action ()
    , setBusy :: RegId -> Action ()
    , ready :: RegId -> Bit 1
    , read :: RegId -> Bit 32
    , write :: RegId -> Bit 32 -> Action ()}

makeRegisterFile :: Module RegisterFile
makeRegisterFile = do
  scoreboard :: Reg (Bit 32) <- makeReg 0
  registers :: RegFile RegId (Bit 32) <- makeRegFile

  readyUpdate :: Wire RegId <- makeWire dontCare
  busyUpdate :: Wire RegId <- makeWire dontCare

  regUpdate :: Wire (RegId,Bit 32) <- makeWire dontCare

  always do
    when (regUpdate.active) do
      let (reg, val) = regUpdate.val
      registers.update reg val

    let ready = zeroExtend readyUpdate.active .<<. readyUpdate.val
    let busy = zeroExtend busyUpdate.active .<<. busyUpdate.val
    scoreboard <== (scoreboard.val .&. inv ready) .|. busy

  return
    RegisterFile
      { setBusy= \ x -> busyUpdate <== x
      , setReady= \ x -> readyUpdate <== x
      , write= \ x v -> regUpdate <== (x,v)
      , ready= \ x ->
          (((1 :: Bit 32) .<<. x) .&. scoreboard.val) === 0 .||.
          (readyUpdate.val === x .&&. readyUpdate.active)
      , read= \ x ->
          (regUpdate.active .&&. regUpdate.val.fst === x) ? (regUpdate.val.snd, registers!x)}

data CoreConfig =
  CoreConfig
    { fetchSource :: Bit (SourceWidth TLConfig)
    , dataSource :: Bit (SourceWidth TLConfig)
    , mmioSource :: Bit (SourceWidth TLConfig)
    , itlbSource :: Bit (SourceWidth TLConfig)
    , dtlbSource :: Bit (SourceWidth TLConfig)
    , hartId :: Integer }

makeCore ::
  CoreConfig
  -> SystemInputs
  -> Module (TLMaster TLConfig, TLMaster TLConfig)
makeCore
  CoreConfig{hartId,fetchSource,dataSource,mmioSource,itlbSource,dtlbSource}
  systemInputs = mdo
  aluQ :: Queue ExecInput <- withName "alu" $ makePipelineQueue 1
  lsuQ :: Queue ExecInput <- withName "lsu_inputs" $ makePipelineQueue 1
  systemQ :: Queue ExecInput <- withName "system" $ makePipelineQueue 1

  redirectQ :: Queue Redirection <- withName "fetch" makeQueue

  window :: Queue DecodeOutput <- withName "pipeline" $ makeSizedQueueCore 4

  inactivityCounter :: Ehr (Bit 16) <- makeEhr 2 0

  always do
    inactivityCounter.write 0 (inactivityCounter.read 0 + 1)
    when (inactivityCounter.read 0 === ones) do
      display "deadlock detected"
      finish

  (imaster, fetch, trainHit, trainMis, itlbFlush) <- withName "fetch" $
    makeFetch systemUnit.vmInfo fetchSource itlbSource (toStream redirectQ)
  decode <- withName "decode" $ makeDecode fetch

  alu <- withName "alu" $ makeAlu (toStream aluQ)
  (mmuSlave, dmaster, Server{reqs= lsuIn, resps= lsuOut}) <-
    withName "lsu" $
      makeLoadStoreUnit
        systemUnit.vmInfo
        dataSource
        dtlbSource
        mmioSource

  (mmuOut, dtlbFlush) <- makeAGU systemUnit.vmInfo dtlbSource mmuSlave (toSource lsuQ)

  registers <- withName "registers" makeRegisterFile

  tlbFlush <- makeDReg false
  always do
    when tlbFlush.val do
      itlbFlush
      dtlbFlush

  systemUnit <- withName "system" $ makeSystem hartId (tlbFlush <== true) systemInputs

  exceptionQ :: Queue (Bit 32, CauseException, Bit 32, Epoch) <- makeQueue
  interruptQ :: Queue (Bit 32, CauseInterrupt, Epoch) <- makeQueue

  writeFromLSU :: Wire (Bit 1) <- makeWire false

  always do
    when (exceptionQ.canDeq .&&. redirectQ.notFull) do
      let (pc, cause, tval, ep) = exceptionQ.first
      trapPc <- systemUnit.exception pc cause tval
      redirectQ.enq Redirection{pc=trapPc, epoch=ep}
      exceptionQ.deq

      --display
      --  "\texception at pc= 0x" (formatHex 0 pc)
      --  " to pc= 0x" (formatHex 0 trapPc)
      --  " " cause

    when (interruptQ.canDeq) do
      let (pc, cause, ep) = interruptQ.first
      trapPc <- systemUnit.interrupt pc cause dontCare
      redirectQ.enq Redirection{pc=trapPc, epoch=ep}
      interruptQ.deq

      --display
      --  "\tinterrupt at pc= 0x" (formatHex 0 pc)
      --  " to pc= 0x" (formatHex 0 trapPc)
      --  " " cause


  epoch :: Ehr Epoch <- makeEhr 2 0

  cycle :: Reg (Bit 32) <- makeReg 0

  withName "pipeline" $ always do
    cycle <== cycle.val + 1

    when (lsuOut.canPeek) do
      let (rd, val) = lsuOut.peek
      when (rd =!= 0) do
        when (hartId == 0 && debug) do
          display "\t\t" hartId "@" (fshowRegId rd) " := 0x" (formatHex 8 val)
        registers.write rd val
      registers.setReady rd
      writeFromLSU <== true
      lsuOut.consume

    when (decode.canPeek .&&. window.notFull) do
      let instr :: Instr = decode.peek.instr
      let rs1 :: RegId = instr.rs1.valid ? (instr.rs1.val, 0)
      let rs2 :: RegId = instr.rs2.valid ? (instr.rs2.val, 0)
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)

      let rdy =
            registers.ready rs1 .&&. registers.ready rs2 .&&. registers.ready rd .&&.
            (instr.isMemAccess ? (lsuQ.notFull, instr.isSystem ? (systemQ.notFull, aluQ.notFull)))

      let input =
            ExecInput
              { pc=decode.peek.pc
              , rs1= registers.read rs1
              , rs2= registers.read rs2
              , instr }

      when (decode.peek.epoch =!= epoch.read 1) do
        decode.consume

      when (rdy .&&. decode.peek.epoch === epoch.read 1) do
        if instr.isMemAccess then do
          lsuQ.enq input
        else do
          if instr.isSystem then do
            systemQ.enq input
          else do
            aluQ.enq input

        decode.consume
        window.enq decode.peek
        when (rd =!= 0) do
          registers.setBusy rd

        --when (hartId == 0) do
        --  display
        --    "\t[" hartId "@" cycle.val "] enter pc: 0x"
        --    (formatHex 8 decode.peek.pc) " instr: " (fshow instr)

    let canRedirect = redirectQ.notFull .&&. exceptionQ.notFull .&&. interruptQ.notFull

    when (window.canDeq .&&. canRedirect .&&. lsuIn.canPut .&&. inv writeFromLSU.val) do
      let req :: DecodeOutput = window.first
      let instr :: Instr = req.instr
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)

      let rdy =
            instr.isMemAccess ?
              ( mmuOut.canPeek
              , instr.isSystem ? (systemQ.canDeq, alu.canPeek))

      when rdy do
        window.deq
        when (inv instr.isMemAccess .||. req.epoch =!= epoch.read 0 .||. mmuOut.peek.exception) do
          registers.setReady rd

        if instr.isMemAccess then do
          mmuOut.consume
        else do
          if instr.isSystem then do
            systemQ.deq
          else do
            alu.consume

        when (req.epoch === epoch.read 0) do
          --when (instr.opcode === 0) do
          --  display "exec invalid instruction at pc= 0x" (formatHex 0 req.pc)

          resp <- do
            if instr.isSystem then do
              systemUnit.exec systemQ.first
            else if instr.isMemAccess then do
              return
                ExecOutput
                  { exception= mmuOut.peek.exception
                  , cause= mmuOut.peek.cause
                  , tval= mmuOut.peek.tval
                  , pc= mmuOut.peek.pc + 4
                  , rd= dontCare
                  , flush= false }
            else do
              return alu.peek

          systemUnit.instret

          inactivityCounter.write 1 0

          let exception :: Bit 1 =
                resp.exception .||. req.exception

          let interrupt :: Bit 1 =
                systemUnit.canInterrupt.valid .&&. inv instr.isMemAccess .&&. inv instr.isSystem

          if exception then do
            let tval = req.exception ? (req.pc, resp.tval)
            let cause = req.exception ? (req.cause, resp.cause)
            exceptionQ.enq (req.pc, cause, tval, epoch.read 0 + 1)
            epoch.write 0 (epoch.read 0 + 1)

          else if interrupt then do
            let cause = systemUnit.canInterrupt.val
            interruptQ.enq (req.pc, cause, epoch.read 0 + 1)
            epoch.write 0 (epoch.read 0 + 1)

          else do
            when (hartId == 0 && debug) do
              display
                "\t[" hartId "@" cycle.val "] retire pc: "
                (formatHex 8 req.pc) " instr: " (fshow instr)

            when (instr.isMemAccess) do
              lsuIn.put mmuOut.peek

            when (rd =!= 0 .&&. inv instr.isMemAccess) do
              when (hartId == 0 && debug) do
                display "\t\t" hartId "@" (fshowRegId rd) " := 0x" (formatHex 8 resp.rd)
              registers.write rd resp.rd

            if (resp.pc =!= req.prediction .||. resp.flush) then do
              --display "redirect to pc := 0x" (formatHex 8 resp.pc)
              redirectQ.enq Redirection{pc= resp.pc, epoch= epoch.read 0 + 1}
              trainMis req.bstate req.pc resp.pc (some instr)
              epoch.write 0 (epoch.read 0 + 1)
            else do
              trainHit req.bstate req.pc resp.pc (some instr)

  return (imaster, dmaster)

makeSpiMmio :: Bit 32 -> Module (SpiFabric, [Mmio TLConfig])
makeSpiMmio baseAddr = do
  outputs :: Queue (Bit 8) <- makeQueue
  (io, inputs) <- makeSpi (toSource outputs)

  let canPeek :: Bit 8 = zeroExtend inputs.canPeek
  let canPut :: Bit 8 = zeroExtend outputs.notFull
  div :: Reg (Bit 32) <- makeReg 32

  always do
    io.setDivider div.val

  let spiMmio =
        Mmio
          { address= baseAddr
          , read= \ mask -> do
              when (at @0 mask .&&. inputs.canPeek) do
                --display "consume spi mmio"
                inputs.consume
              return $ (0 :: Bit 8) # canPeek # canPut # inputs.peek
          , write= \ lane mask -> do
              when (at @0 mask .&&. outputs.notFull) do
                --display "send spi: " (formatHex 0 (slice @7 @0 lane))
                outputs.enq (slice @7 @0 lane)
              when (at @3 mask) do
                --display "set spi cs: " (at @24 lane)
                io.setCS (at @24 lane)}
  let divMmio = regToMmio (baseAddr + 4) div

  return (io.fabric, [spiMmio, divMmio])


makeUartMmio :: Integer -> Bit 1 -> Bit 8 -> Module (Bit 1, Bit 1, Bit 8, [Mmio TLConfig])
makeUartMmio cycles rx btn = do
  inputs <- makeRxUart cycles rx

  outputQ :: Queue (Bit 8) <- makeQueue
  tx <- makeTxUart cycles (toSource outputQ)

  leds :: Reg (Bit 8) <- makeReg 0

  let canPeek :: Bit 8 = zeroExtend inputs.canPeek
  let canPut :: Bit 8 = zeroExtend outputQ.notFull

  let uartMmio =
        Mmio
          { address= 0x10000000
          , read= \ mask -> do
              when (at @0 mask .&&. inputs.canPeek) do
                inputs.consume
              return $ (0::Bit 8) # canPeek # canPut # inputs.peek
          , write= \ lane mask -> do
              when (at @0 mask .&&. outputQ.notFull) do
                outputQ.enq (slice @7 @0 lane)
                --displayAscii (slice @7 @0 lane)
          }

  let ledsMmio =
        Mmio
          { address= 0x10000004
          , read= \_ -> pure ((0 :: Bit 24) # btn)
          , write= \ lane mask -> do
              when (at @0 mask) do
                leds <== slice @7 @0 lane
                display_ "leds <= 0b"
                sequence_ [ display_ x | x <- reverse $ toBitList (slice @7 @0 lane) ]
                display "" }

  return (tx, inputs.canPeek, leds.val, [uartMmio, ledsMmio])

makeTestCore :: Bit 1 -> Module (Bit 1, Bit 8, SpiFabric)
makeTestCore rx = mdo
  (tx, uartInterrupt, leds, uartMmio) <- makeUartMmio 217 rx 0
  (spi, spiMmio) <- makeSpiMmio 0x10001000

  let config =
        TLRAMConfig
          { fileName= Just "Mem.hex"
          , lowerBound= 0x80000000
          , bypassChannelA= False
          , bypassChannelD= True
          , sink= 0 }

  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ x -> x .>=. 0x80000000 ? (0,1)
          , rootSink= \ x -> 0
          , rootSource= \ x ->
              select
                [ x === 0 --> 0
                , x === 1 --> 1
                , x === 2 --> 1
                , x === 3 --> 0
                , x === 4 --> 1
                , x === 5 --> 2
                , x === 6 --> 3
                , x === 7 --> 3
                , x === 8 --> 2
                , x === 9 --> 3 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master0,master1], [slave0,slave1,slave2,slave3]) <-
    withName "xbar" $ makeTLXBar @2 @4 @TLConfig xbarconfig
  uncoherentSlave <- withName "memory" $ makeTLRAM @28 @TLConfig' config
  --uncoherentSlave <- withName "memory" $ makeTLRAM @15 @TLConfig' config

  withName "xbar" $ makeConnection master0 slave
  withName "xbar" $ makeConnection imaster0 slave0
  withName "xbar" $ makeConnection dmaster0 slave1
  --withName "xbar" $ makeConnection imaster1 slave2
  --withName "xbar" $ makeConnection dmaster1 slave3
  withName "xbar" $ makeConnection uncoherentMaster uncoherentSlave

  (clintMmio, clint) <- withName "clint" $ makeClint @TLConfig 2 0x2000000
  clintSlave <- makeTLMmio @TLConfig 1 (clintMmio ++ uartMmio ++ spiMmio)
  withName "clint" $ makeConnection master1 clintSlave

  let systemInputs0 =
        SystemInputs
          { softwareInterrupt= clint.softwareInterrupt
          , timerInterrupt= clint.timerInterrupt!(0 :: Int)
          , externalInterrupt= uartInterrupt }

  let systemInputs1 =
        SystemInputs
          { softwareInterrupt= clint.softwareInterrupt
          , timerInterrupt= clint.timerInterrupt!(1 :: Int)
          , externalInterrupt= uartInterrupt }

  let coreconfig0 =
        CoreConfig
          { fetchSource= 0
          , dataSource= 1
          , mmioSource= 2
          , itlbSource= 3
          , dtlbSource= 4
          , hartId= 0 }
  (imaster0, dmaster0) <- withName "core0" $ makeCore coreconfig0 systemInputs0

  let coreconfig1 =
        CoreConfig
          { fetchSource= 5
          , dataSource= 6
          , mmioSource= 7
          , itlbSource= 8
          , dtlbSource= 9
          , hartId= 1 }
  --(imaster1, dmaster1) <- withName "core1" $ makeCore coreconfig1 systemInputs1

  let bconfig =
        BroadcastConfig
          { sources=
              [ coreconfig0.fetchSource
              , coreconfig0.dataSource ]
              -- , coreconfig1.fetchSource
              -- , coreconfig1.dataSource ]
          , logSize= 6
          , baseSink= 0 }
  (slave, uncoherentMaster) <- withName "broadcast" $ makeBroadcast @TLConfig bconfig

  return (tx, leds, spi)
