module Alu where

import Blarney
import Blarney.Option

import Instr
import Utils
import Ehr

data AluRequest = AluRequest {
    -- Decoded instruction
    instr :: Instr,

    -- First operand
    rs1 :: Bit 32,

    -- Second operand
    rs2 :: Bit 32,

    -- Program counter
    pc  :: Bit 32
  } deriving(Generic, Bits)

data AluResponse = AluResponse {
    exception :: Bit 1,
    cause :: Bit 4,
    tval :: Bit 32,
    rd :: Bit 32,
    pc :: Bit 32
  }

alu :: AluRequest -> AluResponse
alu query =
  AluResponse {rd, exception, cause, pc= newPc, tval= newPc}
  where
    rs1 = query.rs1
    rs2 = query.rs2
    op2 = query.instr.rs2.valid ? (rs2,imm)
    opcode = query.instr.opcode
    imm = query.instr.imm.val
    pc = query.pc

    cause = 1

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
        opcode `is` [BEQ,BNE,BLTU,BGEU,BLT,BGE] --> taken ? (pc+imm,pc+4),
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
