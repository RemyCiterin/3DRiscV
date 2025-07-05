module Core where

import Blarney
import Blarney.Ehr
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Utils
import Blarney.ADT


import Prediction
import System
import Instr
import Clint
import Uart
import Alu
import CSR

import TileLink
import TileLink.CoherentBCache
import TileLink.Broadcast

displayAscii :: Bit 8 -> Action ()
displayAscii term =
  display_ $ go term $ 10 : 13 : [32..126]
  where
    ascii :: [String] = [[toEnum i] | i <- [0..255]]

    go :: Bit 8 -> [Integer] -> Format
    go x [] = formatCond false (fshow "")
    go x (i : is)  =
      formatCond (fromInteger i === x) (fshow (ascii!i)) <>
      go x is

isCached :: Bit 32 -> Bit 1
isCached addr = addr .>=. 0x80000000

type EpochWidth = 8
type Epoch = Bit EpochWidth

type RasSize = 3
type HistSize = 8

data FetchOutput = FetchOutput {
    bstate :: BPredState HistSize RasSize,
    prediction :: Bit 32,
    instr :: Bit 32,
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

data DecodeOutput = DecodeOutput {
    bstate :: BPredState HistSize RasSize,
    prediction :: Bit 32,
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

makeFetch ::
  Bit (SourceWidth TLConfig) ->
  Stream Redirection ->
  Module (TLMaster TLConfig, Stream FetchOutput, Training, Training)
makeFetch source redirection = do
  bpred :: BranchPredictor HistSize RasSize EpochWidth <-
    withName "bpred" $ makeBranchPredictor 10

  key :: Reg (Bit 20) <- makeReg dontCare
  (cache,master) <- withName "icache" $
    makeBCacheCore @2 @20 @6 @4 @() @TLConfig source (\ _ _ -> dontCare)
  responseQ <- makeSizedQueueCore 4

  always do
    when (cache.canMatch) do
      cache.match key.val

    when (cache.loadResponse.canPeek .&&. responseQ.notFull) do
      responseQ.enq cache.loadResponse.peek
      cache.loadResponse.consume

  pc :: Ehr (Bit 32) <- makeEhr 2 0x80000000
  epoch :: Ehr Epoch <- makeEhr 2 0

  queue :: Queue () <- makePipelineQueue 1

  fetchQ :: Queue (Option (Bit 32, Bit 32, BPredState HistSize RasSize, Epoch)) <-
    makeSizedQueueCore 4

  outputQ :: Queue FetchOutput <- makeQueue

  always do
    when (cache.canLookup .&&. queue.notFull) do
      cache.lookup (slice @11 @6 (pc.read 1)) (slice @5 @2 (pc.read 1)) (tag #Load ())
      bpred.start (pc.read 1) (epoch.read 1)
      key <== truncateLSB (pc.read 1)
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
          instr= responseQ.first,
          prediction= outPred,
          bstate= outState,
          epoch= outEpoch,
          pc= outPc
        }

      responseQ.deq
      fetchQ.deq

    when redirection.canPeek do
      pc.write 0 redirection.peek.pc
      epoch.write 0 redirection.peek.epoch
      redirection.consume

  return (master, toStream outputQ, bpred.trainHit, bpred.trainMis)

makeDecode :: Stream FetchOutput -> Module (Stream DecodeOutput)
makeDecode stream = do
  outputQ :: Queue DecodeOutput <- makeQueue

  always do
    when (stream.canPeek .&&. outputQ.notFull) do
      let req = stream.peek
      outputQ.enq DecodeOutput
        { instr= decodeInstr req.instr
        , prediction= req.prediction
        , bstate= req.bstate
        , epoch= req.epoch
        , pc=req.pc }
      stream.consume

  return (toStream outputQ)

makeLoadStoreUnit ::
  Bit (SourceWidth TLConfig) ->
  Bit (SourceWidth TLConfig) ->
  Stream ExecInput ->
  Stream (Bit 1) ->
  Module (TLMaster TLConfig, Stream ExecOutput)
makeLoadStoreUnit cacheSource mmioSource input commit = do
  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ _ -> 0
          , rootSink= \ _ -> 0
          , rootSource= \ x -> x === cacheSource ? (0,1)
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master], [cacheSlave,mmioSlave]) <- makeTLXBar @1 @2 @TLConfig xbarconfig

  outputQ :: Queue ExecOutput <- makeQueue

  key :: Reg (Bit 20) <- makeReg dontCare
  cache <-
    withName "dcache" $
      makeBCacheCoreWith @2 @20 @6 @4 @_ @TLConfig cacheSource cacheSlave execAMO

  always do
    when (cache.canMatch) do
      cache.match key.val

  state :: Reg (Bit 4) <- makeReg 0

  let req = input.peek
  let instr = req.instr
  let opcode = instr.opcode
  let addr = req.rs1 + req.instr.imm.val

  let isWord = instr.accessWidth === 0b10
  let isHalf = instr.accessWidth === 0b01
  let isByte = instr.accessWidth === 0b00

  let aligned =
        selectDefault false [
          opcode `is` [LOAD,STORE] .&&. isByte --> true,
          opcode `is` [LOAD,STORE] .&&. isHalf .&&. at @0 addr === 0 --> true,
          opcode `is` [LOAD,STORE] .&&. isWord .&&. slice @1 @0 addr === 0 --> true,
          opcode `is` [STOREC,LOADR] .&&. slice @1 @0 addr === 0 .&&. isCached addr --> true,
          instr.isAMO .&&. slice @1 @0 addr === 0 .&&. isCached addr --> true
        ]

  let beat = req.rs2 .<<. ((slice @1 @0 addr) # (0b000 :: Bit 3))
  let mask :: Bit 4 =
        select [
          isByte --> 0b0001,
          isHalf --> 0b0011,
          isWord --> 0b1111
        ] .<<. (slice @1 @0 addr)

  -- no speculative MMIO loads, other accessses can start loads before commit
  let canLoad = isCached addr ? (cache.canLookup, mmioSlave.channelA.canPut .&&. commit.canPeek)
  let canStore = cache.canLookup .&&. mmioSlave.channelA.canPut

  let lookup op = do
        cache.lookup (slice @11 @6 addr) (slice @5 @2 addr) op
        key <== truncateLSB addr

  let mmioRequest :: ChannelA TLConfig =
        ChannelA
          { opcode= dontCare
          , source= mmioSource
          , size= zeroExtend instr.accessWidth
          , address= addr
          , lane= dontCare
          , mask= mask }

  let sendLoad op = do
        if isCached addr then do
          lookup op
        else do
          mmioSlave.channelA.put
            (mmioRequest{opcode= tag #Get ()} :: ChannelA TLConfig)

  let canReceiveLoad = isCached addr ? (cache.loadResponse.canPeek, mmioSlave.channelD.canPeek)

  let responseLoad = isCached addr ? (cache.loadResponse.peek, mmioSlave.channelD.peek.lane)

  let consumeLoad =
        if isCached addr then do
          cache.loadResponse.consume
        else do
          mmioSlave.channelD.consume

  let sendStore =
        if isCached addr then do
          lookup (tag #Store (mask,beat))
        else do
            mmioSlave.channelA.put
              (mmioRequest{opcode= tag #PutData (), lane= beat} :: ChannelA TLConfig)

  let canReceiveStore = mmioSlave.channelD.canPeek
  let consumeStore = mmioSlave.channelD.consume

  let out =
        ExecOutput
          { exception= inv aligned
          , pc= req.pc + 4
          , cause= dontCare
          , flush= false
          , rd= dontCare
          , tval= addr }

  always do
    -- *** STORE ***
    when (input.canPeek .&&. opcode `is` [STORE]) do

      when (state.val === 0 .&&. outputQ.notFull) do
        outputQ.enq out{ cause= store_amo_address_misaligned }

        state <== 1

      when (state.val === 1 .&&. commit.canPeek .&&. canStore) do
        commit.consume

        if (commit.peek) then do
          when (addr === 0x10000000) $ displayAscii (slice @7 @0 beat)
          --when (addr === 0x10000000 .&&. slice @7 @0 beat === 0) finish
          when (isCached addr) input.consume
          state <== isCached addr ? (0, 2)
          sendStore
        else do
          input.consume
          state <== 0

      when (state.val === 2 .&&. canReceiveStore) do
        input.consume
        consumeStore
        state <== 0

    -- *** LOAD / LOAD RESERVE ***
    when (input.canPeek .&&. opcode `is` [LOAD,LOADR]) do
      let op = opcode `is` [LOAD] ? (tag #Load (), tag #LoadR ())

      when (state.val === 0 .&&. canLoad) do
        if (aligned .&&. (isCached addr .||. commit.peek)) then do
          -- perform uncached operation when speculation is resolved
          sendLoad op
          state <== 1
        else do
          outputQ.enq out{cause=load_address_misaligned}
          state <== 2

      when (state.val === 1 .&&. canReceiveLoad .&&. outputQ.notFull) do
        consumeLoad

        let beat :: Bit 32 = responseLoad .>>. ((slice @1 @0 addr) # (0b000 :: Bit 3))
        let rd :: Bit 32 =
              select [
                isByte .&&. req.instr.isUnsigned --> zeroExtend (slice @7 @0 beat),
                isHalf .&&. req.instr.isUnsigned --> zeroExtend (slice @15 @0 beat),
                isByte .&&. inv req.instr.isUnsigned --> signExtend (slice @7 @0 beat),
                isHalf .&&. inv req.instr.isUnsigned --> zeroExtend (slice @15 @0 beat),
                isWord --> beat
              ]

        outputQ.enq (out{rd} :: ExecOutput)
        state <== 2

      when (state.val === 2 .&&. commit.canPeek) do
        commit.consume
        input.consume
        state <== 0

    -- *** FENCE ***
    when (input.canPeek .&&. opcode `is` [FENCE]) do
      when (state.val === 0 .&&. outputQ.notFull) do
        outputQ.enq out
        state <== 1

      when (state.val === 1 .&&. commit.canPeek) do
          commit.consume
          input.consume
          state <== 0

    -- *** AMO / STORE CONDITIONAL ***
    let amoOrSc = instr.isAMO .||. opcode `is` [STOREC]
    when (input.canPeek .&&. amoOrSc) do
      when (state.val === 0 .&&. outputQ.notFull) do
        if (aligned) then do
          state <== 1
        else do
          state <== 2
          outputQ.enq out{cause=store_amo_address_misaligned}

      when (state.val === 2 .&&. commit.canPeek) do
        -- abort an unaligned atomic operation
        commit.consume
        input.consume
        state <== 0

      when (state.val === 1 .&&. commit.canPeek .&&. inv commit.peek .&&. outputQ.notFull) do
        -- abort atomic operation because of a misspeculation
        outputQ.enq out
        commit.consume
        input.consume
        state <== 0

      when (state.val === 1 .&&. cache.canLookup .&&. commit.canPeek .&&. commit.peek) do
        -- start executing a cached atomic operation when branch prediction is confirmed
        commit.consume
        state <== 2

        if instr.isAMO then do
          lookup (tag #Atomic (opcode, req.rs2))
        else do
          lookup (tag #StoreC (ones, req.rs2))

      when (state.val === 2 .&&. cache.scResponse.canPeek .&&. outputQ.notFull) do
        outputQ.enq (out{rd= zeroExtend $ inv cache.scResponse.peek} :: ExecOutput)
        cache.scResponse.consume
        input.consume
        state <== 0

      when (state.val === 2 .&&. cache.atomicResponse.canPeek .&&. outputQ.notFull) do
        outputQ.enq (out{rd= cache.atomicResponse.peek} :: ExecOutput)
        cache.atomicResponse.consume
        input.consume
        state <== 0

  return (master, toStream outputQ)

makeAlu :: Stream ExecInput -> Module (Stream ExecOutput)
makeAlu input = do
  return Source{
    consume= input.consume,
    canPeek= input.canPeek,
    peek= alu input.peek
  }

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
    , hartId :: Integer }

makeSystemUnit hartId systemInputs inputs commit = do
  systemUnit <- makeSystem hartId systemInputs
  outputs :: Queue ExecOutput <- makeQueue

  always do
    when (inputs.canPeek .&&. commit.canPeek .&&. outputs.notFull) do
      if commit.peek then do
        out <- systemUnit.exec inputs.peek
        outputs.enq out
      else do
        outputs.enq dontCare
      inputs.consume
      commit.consume

  return (toSource outputs, systemUnit)

makeCore ::
  CoreConfig
  -> SystemInputs
  -> Module (TLMaster TLConfig, TLMaster TLConfig)
makeCore CoreConfig{hartId, fetchSource, dataSource, mmioSource} systemInputs = do
  doCommit :: Ehr (Bit 1) <- makeEhr 2 false
  lsuCommitQ :: Queue (Bit 1) <- withName "lsu" makeQueue
  aluQ :: Queue ExecInput <- withName "alu" makeQueue
  lsuQ :: Queue ExecInput <- withName "lsu" makeQueue
  systemQ :: Queue ExecInput <- withName "system" makeQueue
  systemCommitQ :: Queue (Bit 1) <- withName "system" makeQueue
  systemOutputQ :: Queue ExecOutput <- withName "system" makeQueue

  redirectQ :: Queue Redirection <- withName "fetch" makeBypassQueue

  window :: Queue DecodeOutput <- makeSizedQueueCore 5

  (imaster, fetch, trainHit, trainMis) <-
    withName "fetch" $ makeFetch fetchSource (toStream redirectQ)
  decode <- withName "decode" $ makeDecode fetch

  alu <- withName "alu" $ makeAlu (toStream aluQ)
  (dmaster, lsu) <-
    withName "lsu" $ makeLoadStoreUnit dataSource mmioSource (toStream lsuQ) (toStream lsuCommitQ)

  registers <- withName "registers" makeRegisterFile

  (systemOut, systemUnit) <- withName "system" $
    makeSystemUnit hartId systemInputs (toSource systemQ) (toSource systemCommitQ)

  epoch :: Ehr Epoch <- makeEhr 2 0

  cycle :: Reg (Bit 32) <- makeReg 0

  always do
    cycle <== cycle.val + 1

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

        --display
        --  "\t[" cycle.val "] enter pc: 0x"
        --  (formatHex 8 decode.peek.pc) " instr: " (fshow instr)

    when (window.canDeq .&&. redirectQ.notFull .&&. lsuCommitQ.notFull .&&. systemCommitQ.notFull) do
      let req :: DecodeOutput = window.first
      let instr :: Instr = req.instr
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)

      when (inv (doCommit.read 0) .&&. instr.isMemAccess) do
        lsuCommitQ.enq (req.epoch === epoch.read 0)
        doCommit.write 0 true

      when (inv (doCommit.read 0) .&&. instr.isSystem) do
        systemCommitQ.enq (req.epoch === epoch.read 0)
        doCommit.write 0 true

      let rdy =
            instr.isMemAccess ?
              ( lsu.canPeek .&&. doCommit.read 1
              , instr.isSystem ? (systemOut.canPeek .&&. doCommit.read 1, alu.canPeek))

      when rdy do
        window.deq
        registers.setReady rd

        if instr.isMemAccess then do
          doCommit.write 1 false
          lsu.consume
        else do
          if instr.isSystem then do
            doCommit.write 1 false
            systemOut.consume
          else do
            alu.consume

        when (instr.opcode === 0) do
          display "exec invalid instruction at pc= 0x" (formatHex 0 req.pc)

        when (req.epoch === epoch.read 0) do
          let resp = instr.isMemAccess ? (lsu.peek, instr.isSystem ? (systemOut.peek, alu.peek))

          systemUnit.instret

          if resp.exception then do
            trapPc <- systemUnit.exception req.pc resp.cause resp.tval
            redirectQ.enq Redirection{pc=trapPc, epoch= epoch.read 0 + 1}
            epoch.write 0 (epoch.read 0 + 1)

            display
              "exception at pc= 0x" (formatHex 0 req.pc)
              " to pc= 0x" (formatHex 0 trapPc)
          else if systemUnit.canInterrupt.valid .&&. inv instr.isMemAccess then do
            let cause = systemUnit.canInterrupt.val
            trapPc <- systemUnit.interrupt req.pc cause dontCare

            redirectQ.enq Redirection{pc=trapPc, epoch= epoch.read 0 + 1}
            epoch.write 0 (epoch.read 0 + 1)
          else do
            --when (if hartId == 0 then true else false) do
            --  display
            --    "\t[" hartId "@" cycle.val "] retire pc: "
            --    (formatHex 8 req.pc) " instr: " (fshow instr)

            when (rd =!= 0) do
              --when (if hartId == 0 then true else false) do
              --  display "\t\t" hartId "@" (fshowRegId rd) " := 0x" (formatHex 8 resp.rd)
              registers.write rd resp.rd

            if (resp.pc =!= req.prediction .||. resp.flush) then do
              --display "redirect to pc := 0x" (formatHex 8 resp.pc)
              redirectQ.enq Redirection{pc= resp.pc, epoch= epoch.read 0 + 1}
              trainMis req.bstate req.pc resp.pc (some instr)
              epoch.write 0 (epoch.read 0 + 1)
            else do
              trainHit req.bstate req.pc resp.pc (some instr)

  return (imaster, dmaster)

makeUartMmio :: Integer -> Bit 1 -> Module (Bit 1, Bit 1, [Mmio TLConfig])
makeUartMmio cycles rx = do
  inputs <- makeRxUart cycles rx

  outputQ :: Queue (Bit 8) <- makeQueue
  tx <- makeTxUart cycles (toSource outputQ)

  let canPeek :: Reg (Bit 8) = readOnlyReg (zeroExtend inputs.canPeek)
  let canPut :: Reg (Bit 8) = readOnlyReg (zeroExtend outputQ.notFull)

  let value :: Reg (Bit 8) =
        Reg
          { readReg= inputs.peek
          , writeReg= when outputQ.notFull . outputQ.enq }

  return (tx, inputs.canPeek, [])

makeFakeTestCore :: Bit 1 -> Module (Bit 1)
makeFakeTestCore _ = mdo
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
          , rootSink= \ x -> x === 0 ? (0,1)
          , rootSource= \ x ->
              select
                [ x === 0 --> 0
                , x === 1 --> 1
                , x === 2 --> 1
                , x === 3 --> 2
                , x === 4 --> 3
                , x === 5 --> 3 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master0,master1], [slave0,slave1,slave2,slave3]) <-
    withName "xbar" $ makeTLXBar @2 @4 @TLConfig xbarconfig
  uncoherentSlave <- withName "memory" $ makeTLRAM @18 @TLConfig' config

  withName "xbar" $ makeConnection master0 slave
  withName "xbar" $ makeConnection imaster0 slave0
  withName "xbar" $ makeConnection dmaster0 slave1
  withName "xbar" $ makeConnection imaster1 slave2
  withName "xbar" $ makeConnection dmaster1 slave3
  withName "xbar" $ makeConnection uncoherentMaster uncoherentSlave

  (clintMmio0, clint0) <- withName "clint" $ makeClint @TLConfig 0x30000000
  (clintMmio1, clint1) <- withName "clint" $ makeClint @TLConfig 0x30010000
  clintSlave <- makeTLMmio @TLConfig 1 (clintMmio0 ++ clintMmio1)
  withName "clint" $ makeConnection master1 clintSlave

  let systemInputs0 =
        SystemInputs
          { softwareInterrupt= clint0.softwareInterrupt
          , timerInterrupt= clint0.timerInterrupt
          , externalInterrupt= false }

  let systemInputs1 =
        SystemInputs
          { softwareInterrupt= clint1.softwareInterrupt
          , timerInterrupt= clint1.timerInterrupt
          , externalInterrupt= false }

  let coreconfig0 =
        CoreConfig
          { fetchSource= 0
          , dataSource= 1
          , mmioSource= 2
          , hartId= 0 }
  (imaster0, dmaster0) <- withName "core" $ makeCore coreconfig0 systemInputs0

  let coreconfig1 =
        CoreConfig
          { fetchSource= 3
          , dataSource= 4
          , mmioSource= 5
          , hartId= 1 }
  (imaster1, dmaster1) <- withName "core" $ makeCore coreconfig1 systemInputs1

  let bconfig =
        BroadcastConfig
          { sources=
              [ coreconfig0.fetchSource
              , coreconfig0.dataSource
              , coreconfig1.fetchSource
              , coreconfig1.dataSource ]
          , logSize= 6
          , baseSink= 0 }
  (slave, uncoherentMaster) <- withName "broadcast" $ makeBroadcast @TLConfig bconfig

  return imaster0.channelA.canPeek

makeTestCore :: Module ()
makeTestCore = mdo
  _ <- makeFakeTestCore false
  return ()
