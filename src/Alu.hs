module Alu where

import Blarney
import Blarney.Option
import Blarney.TaggedUnion

import Decode
import Utils
import Ehr

data AluQuery = AluQuery {
    instr :: Instr,
    rs1 :: Bit 32,
    rs2 :: Bit 32
  } deriving(Generic, Bits)

alu :: AluQuery -> Bit 32
alu query =
  select [(itype, ival), (rtype, rval)]
  where
    ival = selectDefault 0 [
      (iop === enum ADDI, rs1 .+. imm),
      (iop === enum XORI, rs1 .^. imm),
      (iop === enum ORI, rs1 .|. imm),
      (iop === enum ANDI, rs1 .&. imm),
      (iop === enum SLTI, toSigned rs1 .<. toSigned imm ? (1,0)),
      (iop === enum SLTIU, rs1 .<. imm ? (1,0)),
      (iop === enum SLLI, rs1 .<<. ishamt),
      (iop === enum SRLI, rs1 .>>. ishamt),
      (iop === enum SRAI, rs1 .>>>. ishamt)]

    rval = selectDefault 0 [
      (rop === enum ADD, rs1 .+. rs2),
      (rop === enum SUB, rs1 .-. rs2),
      (rop === enum XOR, rs1 .^. rs2),
      (rop === enum OR, rs1 .|. rs2),
      (rop === enum AND, rs1 .&. rs2),
      (rop === enum SLT, toSigned rs1 .<. toSigned rs2 ? (1,0)),
      (rop === enum SLTU, rs1 .<. rs2 ? (1,0)),
      (rop === enum SLL, rs1 .<<. rshamt),
      (rop === enum SRL, rs1 .>>. rshamt),
      (rop === enum SRA, rs1 .>>>. rshamt)]

    iop :: ROp
    iop = untag #itype query.instr.op

    rop :: ROp
    rop = untag #rtype query.instr.op

    itype :: Bit 1
    itype = query.instr.op `is` #itype

    rtype :: Bit 1
    rtype = query.instr.op `is` #rtype

    imm :: Bit 32
    imm = immediate query.instr

    ishamt :: Bit 5
    ishamt = slice @4 @0 imm

    rshamt :: Bit 5
    rshamt = slice @4 @0 rs2

    rs1 :: Bit 32
    rs1 = query.rs1

    rs2 :: Bit 32
    rs2 = query.rs2
