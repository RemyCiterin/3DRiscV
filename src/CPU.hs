module CPU where

import Blarney


-- Opcode   | Meaning
-- ---------+----------------------------------
-- 00DDNNNN | reg[DD] <- 0000NNNN
-- 01DDBBAA | reg[DD] <- reg[AA] + reg[BB]
-- 10NNNNAA | pc <- pc - NNNN if reg[BB] != 0
-- 11NNNNNN | halt

type Instr = Bit 8

type RegId = Bit 2

opcode :: Instr -> Bit 2
opcode instr = slice @7 @6 instr

rA :: Instr -> RegId
rA instr = slice @1 @0 instr

rB :: Instr -> RegId
rB instr = slice @3 @2 instr

rD :: Instr -> RegId
rD instr = slice @5 @4 instr

imm :: Instr -> Bit 4
imm instr = slice @3 @0 instr

offset :: Instr -> Bit 4
offset instr = slice @5 @2 instr

makeCPU :: Module ()
makeCPU = do
  instrMem :: RAM (Bit 8) Instr <- makeRAMInit "instrs.hex"

  regFileA :: RAM RegId (Bit 8) <- makeDualRAMForward
  regFileB :: RAM RegId (Bit 8) <- makeDualRAMForward

  instr :: Reg (Bit 8) <- makeReg dontCare

  opA :: Reg (Bit 8) <- makeReg dontCare
  opB :: Reg (Bit 8) <- makeReg dontCare

  pcNext :: Wire (Bit 8) <- makeWire 0
  let pc = delay 0 pcNext.val

  result :: Wire (Bit 8) <- makeWire 0

  flush :: Wire (Bit 1) <- makeWire 0

  count :: Reg (Bit 32) <- makeReg 0
  always do count <== count.val + 1

  go1 :: Reg (Bit 1) <- makeDReg 0
  go2 :: Reg (Bit 1) <- makeDReg 0
  go3 :: Reg (Bit 1) <- makeDReg 0

  always do
    -- Stage 0: Fetch
    instrMem.load pcNext.val

    go1 <== 1

    -- Stage 1: Operand fetch
    when go1.val do
      when (inv flush.val) do
        pcNext <== pc + 1
        go2 <== 1

    regFileA.load (rA instrMem.out)
    regFileB.load (rB instrMem.out)

    instr <== old instrMem.out

    let forward rS other =
          (result.active .&. (rD instr.val === rS (old instrMem.out))) ?
          (result.val, other)

    opA <== forward rA regFileA.out
    opB <== forward rB regFileB.out

    when (inv flush.val) do
      go3 <== go2.val

    -- Instruction dispatch
    when go3.val do
      switch (opcode instr.val)
        [
          0b00 --> result <== zeroExtend (imm instr.val),
          0b01 --> result <== opA.val + opB.val,
          0b10 --> when (opB.val =!= 0) do
            pcNext <== pc - zeroExtend (offset instr.val) - 2
            flush <== 1,
          0b11 --> finish
        ]

    -- Stage 3: Write-back
    when result.active do
      regFileA.store (rD instr.val) result.val
      regFileB.store (rD instr.val) result.val
      display (formatDec 8 count.val)
        ": rf [" (rD instr.val) "] := " result.val
