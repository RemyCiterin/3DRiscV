module Core where

import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream

import Prediction
import Cache
import Utils
import Instr
import Alu
import Ehr

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

type Training = BPredState HistSize RasSize -> Bit 32 -> Bit 32 -> Option Instr -> Action ()

makeFetch ::
  Stream CpuResponse ->
  Stream Redirection ->
  Module (Stream CpuRequest, Stream FetchOutput, Training, Training)
makeFetch response redirection = do
  bpred :: BranchPredictor HistSize RasSize EpochWidth <- makeBranchPredictor 10

  pc :: Ehr (Bit 32) <- makeEhr 2 0x80000000
  epoch :: Ehr Epoch <- makeEhr 2 0

  queue :: Queue () <- makePipelineQueue 1

  responseQ :: Queue (Bit (32)) <- makeSizedQueue 4
  fetchQ :: Queue (Option (Bit 32, Bit 32, BPredState HistSize RasSize, Epoch)) <- makeSizedQueue 4

  requestQ :: Queue CpuRequest <- makeBypassQueue
  outputQ :: Queue FetchOutput <- makeQueue

  always do
    when (responseQ.notFull .&&. response.canPeek) do
      responseQ.enq response.peek.beat
      response.consume

    when (requestQ.notFull .&&. queue.notFull) do
      requestQ.enq CpuRequest{read= true, addr= pc.read 1, beat= dontCare, mask= dontCare}
      bpred.start (pc.read 1) (epoch.read 1)
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

  return (toStream requestQ, toStream outputQ, bpred.trainHit, bpred.trainMis)

makeDecode :: Stream FetchOutput -> Module (Stream DecodeOutput)
makeDecode stream = do
  outputQ :: Queue DecodeOutput <- makeQueue

  always do
    when (stream.canPeek .&&. outputQ.notFull) do
      let req = stream.peek
      outputQ.enq DecodeOutput{
        instr= decodeInstr req.instr,
        prediction= req.prediction,
        bstate= req.bstate,
        epoch= req.epoch,
        pc=req.pc}
      stream.consume

  return (toStream outputQ)

makeLoadStoreUnit ::
  Stream ExecInput ->
  Stream (Bit 1) ->
  Stream CpuResponse ->
  Module (Stream CpuRequest, Stream ExecOutput)
makeLoadStoreUnit input commit response = do
  outputQ :: Queue ExecOutput <- makeQueue
  requestQ :: Queue CpuRequest <- makeQueue

  state :: Reg (Bit 3) <- makeReg 0

  let req = input.peek
  let opcode = req.instr.opcode
  let addr = req.rs1 + req.instr.imm.val

  let isWord = req.instr.accessWidth === 0b10
  let isHalf = req.instr.accessWidth === 0b01
  let isByte = req.instr.accessWidth === 0b00

  let aligned =
        selectDefault false [
          isByte --> true,
          isHalf .&&. at @0 addr === 0 --> true,
          isWord .&&. slice @1 @0 addr === 0 --> true
        ]

  always do
    when (state.val === 0 .&&. input.canPeek .&&. opcode `is` [STORE] .&&. outputQ.notFull) do
      outputQ.enq ExecOutput{
        exception= inv aligned,
        pc= req.pc + 4,
        tval= addr,
        cause= 6,
        rd= 0
      }

      state <== 3

    when (state.val === 3 .&&. requestQ.notFull .&&. commit.canPeek) do
      commit.consume

      let beat = req.rs2 .<<. ((slice @1 @0 addr) # (0b000 :: Bit 3))
      let mask :: Bit 4 =
            select [
              isByte --> 0b0001,
              isHalf --> 0b0011,
              isWord --> 0b1111
            ] .<<. (slice @1 @0 addr)

      if (commit.peek) then do
        when (addr === 0x10000000) do displayAscii (slice @7 @0 beat)

        requestQ.enq CpuRequest{read= false, mask, addr, beat}
        state <== 4
      else do
        input.consume
        state <== 0

    when (state.val === 4 .&&. response.canPeek) do
      response.consume
      input.consume
      state <== 0

    when (state.val === 0 .&&. input.canPeek .&&. requestQ.notFull .&&. opcode `is` [LOAD]) do
      requestQ.enq CpuRequest{read= true, addr, mask= dontCare, beat= dontCare}
      state <== 1

    when (state.val === 1 .&&. response.canPeek .&&. outputQ.notFull) do
      response.consume

      let beat :: Bit 32 = response.peek.beat .>>. ((slice @1 @0 addr) # (0b000 :: Bit 3))
      let out :: Bit 32 =
            select [
              isByte .&&. req.instr.isUnsigned --> zeroExtend (slice @7 @0 beat),
              isHalf .&&. req.instr.isUnsigned --> zeroExtend (slice @15 @0 beat),
              isByte .&&. inv req.instr.isUnsigned --> signExtend (slice @7 @0 beat),
              isHalf .&&. inv req.instr.isUnsigned --> zeroExtend (slice @15 @0 beat),
              isWord --> beat
            ]

      outputQ.enq ExecOutput{pc=req.pc+4, exception=inv aligned, cause= 4, tval= addr, rd= out}

      state <== 2

    when (state.val === 2 .&&. commit.canPeek) do
      commit.consume
      input.consume
      state <== 0

    when (state.val === 0 .&&. input.canPeek .&&. opcode `is` [FENCE] .&&. outputQ.notFull) do
      outputQ.enq ExecOutput{pc=req.pc+4, exception=false, cause= dontCare, tval= dontCare, rd= 0}
      state <== 5

    when (state.val === 5 .&&. commit.canPeek) do
      commit.consume
      input.consume
      state <== 0

  return (toStream requestQ, toStream outputQ)

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

makeCore ::
  Stream CpuResponse ->
  Stream CpuResponse ->
  Module (Stream CpuRequest, Stream CpuRequest)
makeCore iresponse dresponse = do
  commitQ :: Queue (Bit 1) <- makeQueue
  aluQ :: Queue ExecInput <- makeQueue
  lsuQ :: Queue ExecInput <- makeQueue

  redirectQ :: Queue Redirection <- makeBypassQueue

  window :: Queue DecodeOutput <- makeQueue

  (irequest, fetch, trainHit, trainMis) <- makeFetch iresponse (toStream redirectQ)
  decode <- makeDecode fetch

  alu <- makeAlu (toStream aluQ)
  (drequest, lsu) <- makeLoadStoreUnit (toStream lsuQ) (toStream commitQ) dresponse

  registers <- makeRegisterFile

  epoch :: Ehr Epoch <- makeEhr 2 0

  cycle :: Reg (Bit 32) <- makeReg 0
  instret :: Reg (Bit 32) <- makeReg 0

  always do
    cycle <== cycle.val + 1

    when (decode.canPeek .&&. window.notFull) do
      let instr :: Instr = decode.peek.instr
      let rs1 :: RegId = instr.rs1.valid ? (instr.rs1.val, 0)
      let rs2 :: RegId = instr.rs2.valid ? (instr.rs2.val, 0)
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)

      let rdy =
            registers.ready rs1 .&&. registers.ready rs2 .&&. registers.ready rd .&&.
            (instr.isMemAccess ? (lsuQ.notFull, aluQ.notFull))

      let op1 = registers.read rs1
      let op2 = registers.read rs2

      let input = ExecInput{pc=decode.peek.pc, rs1= op1, rs2= op2, instr}

      when (decode.peek.epoch =!= epoch.read 1) do
        decode.consume

      when (rdy .&&. decode.peek.epoch === epoch.read 1) do
        if instr.isMemAccess then do
          lsuQ.enq input
        else do
          aluQ.enq input

        decode.consume
        window.enq decode.peek
        when (rd =!= 0) do
          registers.setBusy rd

        --display
        --  "\t[" cycle.val "] enter pc: 0x"
        --  (formatHex 8 decode.peek.pc) " instr: " (fshow instr)

    when (window.canDeq .&&. redirectQ.notFull .&&. commitQ.notFull) do
      let req :: DecodeOutput = window.first
      let instr :: Instr = req.instr
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)
      let rdy = instr.isMemAccess ? (lsu.canPeek, alu.canPeek)

      let resp = instr.isMemAccess ? (lsu.peek, alu.peek)

      when rdy do
        window.deq
        registers.setReady rd

        if instr.isMemAccess then do
          commitQ.enq (req.epoch === epoch.read 0)
          lsu.consume
        else do
          alu.consume

        when (req.epoch === epoch.read 0) do
          when (instr.opcode `is` [FENCE]) do
            display "fence at: " cycle.val " instret: " instret.val

          instret <== instret.val + 1

          --display
          --  "\t[" cycle.val "] retire pc: "
          --  (formatHex 8 req.pc) " instr: " (fshow instr)

          when (rd =!= 0) do
            --display "    " (fshowRegId rd) " := 0x" (formatHex 8 resp.rd)
            --registers.update rd resp.rd
            registers.write rd resp.rd

          if (resp.pc =!= req.prediction) then do
            --display "redirect to pc := 0x" (formatHex 8 resp.pc)
            redirectQ.enq Redirection{pc= resp.pc, epoch= epoch.read 0 + 1}
            trainMis req.bstate req.pc resp.pc (some instr)
            epoch.write 0 (epoch.read 0 + 1)
          else do
            trainHit req.bstate req.pc resp.pc (some instr)

  return (irequest, drequest)

makeMem :: Stream CpuRequest -> Module (Stream CpuResponse)
makeMem stream = do
  ram :: RAMBE 16 4 <- makeDualRAMForwardInitBE "Mem.hex"
  responseQ :: Queue CpuResponse <- makeQueue
  queue :: Queue () <- makePipelineQueue 1

  cycle :: Reg (Bit 32) <- makeReg 0

  always do
    cycle <== cycle.val + 1

    when (stream.canPeek .&&. queue.notFull) do
      let (msb, lsb) = split (slice @31 @2 (stream.peek.addr - 0x80000000))

      if stream.peek.read then do
        ram.loadBE lsb
      else do
        when (msb === 0) do
          ram.storeBE lsb stream.peek.mask stream.peek.beat
      stream.consume
      queue.enq ()

    when (queue.canDeq .&&. responseQ.notFull) do
      responseQ.enq CpuResponse{beat= ram.outBE}
      queue.deq

  return (toStream responseQ)

makeTestCore :: Module ()
makeTestCore = mdo
  iresponse <- makeMem irequest
  dresponse <- makeMem drequest
  (irequest, drequest) <- makeCore iresponse dresponse
  return ()

makeFakeTestCore :: Bit 1 -> Module (Bit 1)
makeFakeTestCore _ = mdo
  iresponse <- makeMem irequest
  dresponse <- makeMem drequest
  (irequest, drequest) <- makeCore iresponse dresponse
  return drequest.canPeek
