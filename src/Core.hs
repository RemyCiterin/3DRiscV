module Core where

import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream

import Cache
import Utils
import Instr
import Alu
import Ehr

displayAscii :: Bit 8 -> Action ()
displayAscii term = display_ $ go term $ 10 : 13 : [33..126]
  where
    ascii :: [String] = [[toEnum i] | i <- [0..255]]

    go :: Bit 8 -> [Integer] -> Format
    go x [] = formatCond false (fshow "")
    go x (i : is)  =
      formatCond (fromInteger i === x) (fshow (ascii!i)) <>
      go x is

type Epoch = Bit 8

data FetchOutput = FetchOutput {
    instr :: Bit 32,
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

data DecodeOutput = DecodeOutput {
    instr :: Instr,
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

data Redirection = Redirection {
    epoch :: Epoch,
    pc :: Bit 32
  } deriving(Bits, Generic)

makeFetch ::
  Stream CpuResponse ->
  Stream Redirection ->
  Module (Stream CpuRequest, Stream FetchOutput)
makeFetch response redirection = do
  pc :: Reg (Bit 32) <- makeReg 0x80000000
  epoch :: Reg Epoch <- makeReg 0

  responseQ :: Queue (Bit (32)) <- makeSizedQueue 4
  fetchQ :: Queue (Bit (32), Epoch) <- makeSizedQueue 4

  requestQ :: Queue CpuRequest <- makeBypassQueue
  outputQ :: Queue FetchOutput <- makeQueue

  always do
    when (responseQ.notFull .&&. response.canPeek) do
      responseQ.enq response.peek.beat
      response.consume

    when redirection.canPeek do
      pc <== redirection.peek.pc
      epoch <== redirection.peek.epoch
      redirection.consume

    when (fetchQ.canDeq .&&. responseQ.canDeq .&&. outputQ.notFull) do
      let (outPc, outEpoch) = fetchQ.first
      outputQ.enq FetchOutput{instr= responseQ.first, pc= outPc, epoch= outEpoch}
      responseQ.deq
      fetchQ.deq

    when (requestQ.notFull .&&. fetchQ.notFull .&&. inv redirection.canPeek) do
      requestQ.enq CpuRequest{read= true, addr= pc.val, beat= dontCare, mask= dontCare}
      fetchQ.enq (pc.val, epoch.val)
      pc <== pc.val + 4

  return (toStream requestQ, toStream outputQ)

makeDecode :: Stream FetchOutput -> Module (Stream DecodeOutput)
makeDecode stream = do
  outputQ :: Queue DecodeOutput <- makeQueue

  always do
    when (stream.canPeek .&&. outputQ.notFull) do
      let req = stream.peek
      outputQ.enq DecodeOutput{pc=req.pc, epoch= req.epoch, instr= decodeInstr req.instr}
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
    when (state.val === 0 .&&. opcode `is` [STORE] .&&. outputQ.notFull) do
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

    when (state.val === 0 .&&. requestQ.notFull .&&. opcode `is` [LOAD]) do
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

  return (toStream requestQ, toStream outputQ)

makeAlu :: Stream ExecInput -> Module (Stream ExecOutput)
makeAlu input = do
  return Source{
    consume= input.consume,
    canPeek= input.canPeek,
    peek= alu input.peek
  }


makeCore ::
  Stream CpuResponse ->
  Stream CpuResponse ->
  Module (Stream CpuRequest, Stream CpuRequest)
makeCore iresponse dresponse = do
  commitQ :: Queue (Bit 1) <- makeQueue
  aluQ :: Queue ExecInput <- makeQueue
  lsuQ :: Queue ExecInput <- makeQueue

  redirectQ :: Queue Redirection <- makeBypassQueue

  window :: Queue DecodeOutput <- makeSizedQueue 8

  (irequest, fetch) <- makeFetch iresponse (toStream redirectQ)
  decode <- makeDecode fetch

  alu <- makeAlu (toStream aluQ)
  (drequest, lsu) <- makeLoadStoreUnit (toStream lsuQ) (toStream commitQ) dresponse

  scoreboard :: Ehr (Bit 32) <- makeEhr 2 0
  registers :: [Reg (Bit 32)] <- replicateM 32 (makeReg 0)

  epoch :: Reg Epoch <- makeReg 0

  let ready x = (((1 :: Bit 32) .<<. x) .&. scoreboard.read 0) === 0

  always do
    when (decode.canPeek .&&. window.notFull) do
      let instr :: Instr = decode.peek.instr
      let rs1 :: RegId = instr.rs1.valid ? (instr.rs1.val, 0)
      let rs2 :: RegId = instr.rs2.valid ? (instr.rs2.val, 0)
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)

      let rdy =
            ready rs1 .&&. ready rs2 .&&. ready rd .&&.
              (instr.isMemAccess ? (lsuQ.notFull, aluQ.notFull))

      let op1 = (registers!rs1).val
      let op2 = (registers!rs2).val

      let input = ExecInput{pc=decode.peek.pc, rs1= op1, rs2= op2, instr}

      when rdy do
        if instr.isMemAccess then do
          lsuQ.enq input
        else do
          aluQ.enq input

        decode.consume
        window.enq decode.peek
        when (rd =!= 0) do
          scoreboard.write 0 (scoreboard.read 0 .|. (1 .<<. rd))

        --display "enter pc: " (formatHex 8 decode.peek.pc) " instr: " (fshow instr)

    when (window.canDeq .&&. redirectQ.notFull .&&. commitQ.notFull) do
      let instr :: Instr = window.first.instr
      let rs1 :: RegId = instr.rs1.valid ? (instr.rs1.val, 0)
      let rs2 :: RegId = instr.rs2.valid ? (instr.rs2.val, 0)
      let rd  :: RegId = instr.rd.valid ? (instr.rd.val, 0)
      let rdy = instr.isMemAccess ? (lsu.canPeek, alu.canPeek)

      let resp = instr.isMemAccess ? (lsu.peek, alu.peek)

      when rdy do
        window.deq
        scoreboard.write 1 (scoreboard.read 0 .&. inv (1 .<<. rd))

        if instr.isMemAccess then do
          commitQ.enq (window.first.epoch === epoch.val)
          lsu.consume
        else do
          alu.consume

        when (window.first.epoch === epoch.val) do
          --display "retire pc: " (formatHex 8 window.first.pc) " instr: " (fshow instr)

          when (rd =!= 0) do
            --display "    " (fshowRegId rd) " := 0x" (formatHex 8 resp.rd)
            (registers!rd) <== resp.rd

          when (resp.pc =!= window.first.pc + 4) do
            --display "redirect to pc := 0x" (formatHex 8 resp.pc)
            redirectQ.enq Redirection{pc= resp.pc, epoch= epoch.val+1}
            epoch <== epoch.val + 1

  return (irequest, drequest)

makeMem :: Stream CpuRequest -> Module (Stream CpuResponse)
makeMem stream = do
  responseQ :: Queue CpuResponse <- makeSizedQueue 4
  ram :: RAMBE 28 4 <- makeDualRAMForwardInitBE "Mem.hex"
  read :: Reg (Bit 1) <- makeDReg false

  always do
    when (stream.canPeek .&&. responseQ.notFull) do

      let (msb, lsb) = split (slice @31 @2 stream.peek.addr)
      if stream.peek.read then do
        ram.loadBE lsb
        read <== true
      else do
        when (msb === 0) do
          ram.storeBE lsb stream.peek.mask stream.peek.beat
        read <== true
      stream.consume

    when read.val do
      responseQ.enq CpuResponse{beat= ram.outBE}

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
