module Alu where

import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Utils
import Blarney.ADT
import Blarney.Ehr

import Instr
import MulDiv
import CSR

-- Return a valid execution with pc+4 as output if the instruction
-- is not managedby the ALU (used for multiplier/divider)
alu :: ExecInput -> ExecOutput
alu query =
  ExecOutput {rd, exception, cause, pc= newPc, tval= newPc, flush= false}
  where
    rs1 = query.rs1
    rs2 = query.rs2
    op2 = query.instr.rs2.valid ? (rs2,imm)
    opcode = query.instr.opcode
    imm = query.instr.imm.val
    off = query.instr.off
    pc = query.pc

    cause = instruction_address_misaligned

    exception = slice @1 @0 newPc =!= 0

    taken = select [
        opcode `is` [BEQ] --> rs1 === rs2,
        opcode `is` [BNE] --> rs1 =!= rs2,
        opcode `is` [BLTU] --> rs1 .<. rs2,
        opcode `is` [BGEU] --> rs1 .>=. rs2,
        opcode `is` [BLT] --> toSigned rs1 .<. toSigned rs2,
        opcode `is` [BGE] --> toSigned rs1 .>=. toSigned rs2
      ]

    newPc = selectDefault (pc + 4) [
        opcode `is` [BEQ,BNE,BLTU,BGEU,BLT,BGE] --> taken ? (pc+signExtend off,pc+4),
        opcode `is` [JALR] --> (rs1 + imm) .&. inv 1,
        opcode `is` [JAL]  --> pc + imm
      ]

    rd = select [
        opcode `is` [JALR, JAL] --> pc + 4,
        opcode `is` [LUI] --> imm,
        opcode `is` [AUIPC] --> pc + imm,
        opcode `is` [ADD] --> rs1 + op2,
        opcode `is` [SUB] --> rs1 - op2,
        opcode `is` [XOR] --> rs1 .^. op2,
        opcode `is` [OR] --> rs1 .|. op2,
        opcode `is` [AND] --> rs1 .&. op2,
        opcode `is` [SLT] --> toSigned rs1 .<. toSigned op2 ? (1,0),
        opcode `is` [SLTU] --> rs1 .<. op2 ? (1,0),
        opcode `is` [SLL] --> rs1 .<<. slice @4 @0 op2,
        opcode `is` [SRL] --> rs1 .>>. slice @4 @0 op2,
        opcode `is` [SRA] --> rs1 .>>>. slice @4 @0 op2
      ]

makeAlu :: Source ExecInput -> Module (Source ExecOutput)
makeAlu inputs = do
  Server{reqs=mulIn, resps=mulOut} <- makeMultiplier @64 2
  Server{reqs=divIn, resps=divOut} <- makeDivider @34

  idle :: Reg (Bit 1) <- makeReg true
  always do
    when (idle.val .&&. inputs.canPeek) do
      when (mulIn.canPut .&&. isMul) do
        mulIn.put (mulLhs, mulRhs)
        idle <== false

      when (divIn.canPut .&&. isDivRem) do
        divIn.put (divNum, divDen)
        idle <== false

  return
    Source
      { canPeek=
          inputs.canPeek
          .&&. ((mulOut.canPeek .&&. inv idle.val) .||. inv isMul)
          .&&. ((divOut.canPeek .&&. inv idle.val) .||. inv isDivRem)
      , peek=
          let rd = selectDefault output.rd
                [ divOverflow --> isRem ? (0, rs1)
                , divZero --> isRem ? (0, -1)
                , isDivRem .&&. inv divOverflow .&&. inv divZero -->
                    isRem ? (lower divOut.peek.snd, lower divOut.peek.fst)
                , isMul --> mulUpper ? (upper mulOut.peek, lower mulOut.peek) ] in
          (output{rd} :: ExecOutput)
      , consume= do
          inputs.consume
          when isMul mulOut.consume
          when isDivRem divOut.consume
          when (isMul .||. isDivRem) do
            idle <== true }
  where
    instr = inputs.peek.instr
    output = alu inputs.peek
    opcode = instr.opcode
    rs1 = inputs.peek.rs1
    rs2 = inputs.peek.rs2

    mulLhs, mulRhs :: Bit 64
    mulLhs = opcode `is` [MUL,MULH,MULHSU] ? (signExtend rs1, zeroExtend rs1)
    mulRhs = opcode `is` [MUL,MULH] ? (signExtend rs2, zeroExtend rs2)
    mulUpper = opcode `is` [MULH, MULHSU, MULHU]
    isMul = opcode `is` [MUL,MULH,MULHSU,MULHU]

    divNum, divDen :: Bit 34
    divNum = opcode `is` [DIV,REM] ? (signExtend rs1, zeroExtend rs1)
    divDen = opcode `is` [DIV,REM] ? (signExtend rs2, zeroExtend rs2)
    isDivRem = opcode `is` [DIV,DIVU,REM,REMU]
    isRem = opcode `is` [REM,REMU]

    divOverflow = opcode `is` [DIV,REM] .&&. signedDivOverflow (rs1, rs2)
    divZero = opcode `is` [DIV,DIVU,REM,REMU] .&&. rs2 === 0

execCSR :: Priv -> CSRUnit -> ExecInput -> Action ExecOutput
execCSR currentPriv unit ExecInput{instr, pc, rs1} = do
  let doRead = instr.opcode `is` [CSRRW] ? (instr.rd.val =!= 0, true)
  let doWrite = instr.opcode `is` [CSRRC,CSRRS] ? (instr.rs1.val =!= 0, true)
  let readOnly = isReadOnlyCSR instr.csr

  let legal =
        inv (doWrite .&&. readOnly)
        .&&. currentPriv .>=. minPrivCSR instr.csr

  x <- whenAction (doRead .&&. legal) (unit.csrUnitRead instr.csr)

  let operand = instr.csrI ? (zeroExtend instr.rs1.val, rs1)

  let value =
        select
          [ instr.opcode `is` [CSRRW] --> operand
          , instr.opcode `is` [CSRRS] --> x .|. operand
          , instr.opcode `is` [CSRRC] --> x .&. inv operand ]

  when (legal .&&. doWrite) do
    unit.csrUnitWrite instr.csr value

  return
    ExecOutput
      { rd= x
      , exception= inv legal
      , cause= illegal_instruction
      , tval= instr.raw
      , flush= unit.csrUnitFlush
      , pc= pc + 4 }

execAMO :: (MnemonicVec, Bit 32) -> Bit 32 -> Bit 32
execAMO (opcode, x) y =
  select
    [ opcode `is` [AMOOR] --> x .|. y
    , opcode `is` [AMOAND] --> x .&. y
    , opcode `is` [AMOXOR] --> x .^. y
    , opcode `is` [AMOSWAP] --> x
    , opcode `is` [AMOADD] --> x + y
    , opcode `is` [AMOMIN] --> toSigned x .>. toSigned y ? (y,x)
    , opcode `is` [AMOMAX] --> toSigned x .>. toSigned y ? (x,y)
    , opcode `is` [AMOMINU] --> x .>. y ? (y,x)
    , opcode `is` [AMOMAXU] --> x .>. y ? (x,y) ]
