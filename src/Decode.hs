module Decode where

import Blarney.Option
import Blarney.TaggedUnion
import Blarney
import Utils

data ArchReg = ArchReg (Bit 5) deriving(Generic, Bits, Cmp)

instance FShow ArchReg where
  fshow (ArchReg reg) =
    formatCond (reg === 0) (fshow "zero") <>
    formatCond (reg === 1) (fshow "ra") <>
    formatCond (reg === 2) (fshow "sp") <>
    formatCond (reg === 3) (fshow "gp") <>
    formatCond (reg === 4) (fshow "tp") <>
    formatCond (reg === 5) (fshow "t0") <>
    formatCond (reg === 6) (fshow "t1") <>
    formatCond (reg === 7) (fshow "t2") <>
    formatCond (reg === 8) (fshow "s0") <>
    formatCond (reg === 9) (fshow "s1") <>
    formatCond (reg === 10) (fshow "a0") <>
    formatCond (reg === 11) (fshow "a1") <>
    formatCond (reg === 12) (fshow "a2") <>
    formatCond (reg === 13) (fshow "a3") <>
    formatCond (reg === 14) (fshow "a4") <>
    formatCond (reg === 15) (fshow "a5") <>
    formatCond (reg === 16) (fshow "a6") <>
    formatCond (reg === 17) (fshow "a7") <>
    formatCond (reg === 18) (fshow "s2") <>
    formatCond (reg === 19) (fshow "s3") <>
    formatCond (reg === 20) (fshow "s4") <>
    formatCond (reg === 21) (fshow "s5") <>
    formatCond (reg === 22) (fshow "s6") <>
    formatCond (reg === 23) (fshow "s7") <>
    formatCond (reg === 24) (fshow "s8") <>
    formatCond (reg === 25) (fshow "s9") <>
    formatCond (reg === 26) (fshow "s10") <>
    formatCond (reg === 27) (fshow "s11") <>
    formatCond (reg === 28) (fshow "t3") <>
    formatCond (reg === 29) (fshow "t4") <>
    formatCond (reg === 30) (fshow "t5") <>
    formatCond (reg === 31) (fshow "t6")

data Optype = Optype (Bit 3) deriving(Generic, Bits, Cmp)

itype :: Optype
itype = Optype 0

utype :: Optype
utype = Optype 1

btype :: Optype
btype = Optype 2

stype :: Optype
stype = Optype 3

rtype :: Optype
rtype = Optype 4

jtype :: Optype
jtype = Optype 5

type Opcode = Bit 7

opcodeType :: Opcode -> Option Optype
opcodeType code =
  listIndex (slice @6 @2 code) [
    some itype,
    none,
    none,
    some itype,
    some itype,
    some utype,
    none,
    none,

    some stype,
    none,
    none,
    none,
    some rtype,
    some utype,
    none,
    none,

    none,
    none,
    none,
    none,
    none,
    none,
    none,
    none,

    some btype,
    some itype,
    none,
    some jtype,
    some itype,
    none,
    none,
    none
  ]

type Funct3 = Bit 3
type Funct7 = Bit 7

getOpcode :: Bit 32 -> Opcode
getOpcode instr = slice @6 @0 instr

getFunct3 :: Bit 32 -> Funct3
getFunct3 instr = slice @14 @12 instr

getFunct7 :: Bit 32 -> Funct7
getFunct7 instr = slice @31 @25 instr

getRs1 :: Bit 32 -> ArchReg
getRs1 instr = ArchReg $ slice @19 @15 instr

getRs2 :: Bit 32 -> ArchReg
getRs2 instr = ArchReg $ slice @24 @20 instr

getRd :: Bit 32 -> ArchReg
getRd instr = ArchReg $ slice @11 @7 instr

class HasImmediate t where
  immediate :: t -> Bit 32

class HasOpcode t where
  opcode :: t -> Opcode

class HasRegister1 t where
  register1 :: t -> ArchReg

class HasRegister2 t where
  register2 :: t -> ArchReg

class HasDestination t where
  destination :: t -> ArchReg

class HasFunction3 t where
  function3 :: t -> Funct3

class HasFunction7 t where
  function7 :: t -> Funct7

data Rtype = Rtype (Bit 32) deriving(Generic, Bits, Cmp)
data Itype = Itype (Bit 32) deriving(Generic, Bits, Cmp)
data Stype = Stype (Bit 32) deriving(Generic, Bits, Cmp)
data Btype = Btype (Bit 32) deriving(Generic, Bits, Cmp)
data Utype = Utype (Bit 32) deriving(Generic, Bits, Cmp)
data Jtype = Jtype (Bit 32) deriving(Generic, Bits, Cmp)

instance HasFunction3 Rtype where
  function3 (Rtype instr) = getFunct3 instr
instance HasFunction3 Itype where
  function3 (Itype instr) = getFunct3 instr
instance HasFunction3 Stype where
  function3 (Stype instr) = getFunct3 instr
instance HasFunction3 Btype where
  function3 (Btype instr) = getFunct3 instr

instance HasFunction7 Rtype where
  function7 (Rtype instr) = getFunct7 instr

instance HasImmediate Itype where
  immediate (Itype instr) = signExtend (slice @31 @20 instr)
instance HasImmediate Stype where
  immediate (Stype instr) = signExtend (slice @31 @25 instr # slice @11 @7 instr)
instance HasImmediate Btype where
  immediate (Btype instr) = signExtend (imm # constant @1 0) where
    imm = at @31 instr # at @7 instr # slice @30 @25 instr # slice @11 @8 instr
instance HasImmediate Utype where
  immediate (Utype instr) = signExtend (slice @31 @12 instr # constant @12 0)
instance HasImmediate Jtype where
  immediate (Jtype instr) = signExtend (imm # constant @1 0) where
    imm = at @31 instr # slice @19 @12 instr # at @20 instr # slice @30 @21 instr

instance HasOpcode Rtype where
  opcode (Rtype instr) = getOpcode instr
instance HasOpcode Itype where
  opcode (Itype instr) = getOpcode instr
instance HasOpcode Stype where
  opcode (Stype instr) = getOpcode instr
instance HasOpcode Btype where
  opcode (Btype instr) = getOpcode instr
instance HasOpcode Utype where
  opcode (Utype instr) = getOpcode instr
instance HasOpcode Jtype where
  opcode (Jtype instr) = getOpcode instr

instance HasRegister1 Rtype where
  register1 (Rtype instr) = getRs1 instr
instance HasRegister1 Itype where
  register1 (Itype instr) = getRs1 instr
instance HasRegister1 Stype where
  register1 (Stype instr) = getRs1 instr
instance HasRegister1 Btype where
  register1 (Btype instr) = getRs1 instr

instance HasRegister2 Rtype where
  register2 (Rtype instr) = getRs2 instr
instance HasRegister2 Itype where
  register2 (Itype instr) = getRs2 instr
instance HasRegister2 Stype where
  register2 (Stype instr) = getRs2 instr
instance HasRegister2 Btype where
  register2 (Btype instr) = getRs2 instr

instance HasDestination Rtype where
  destination (Rtype instr) = getRd instr
instance HasDestination Itype where
  destination (Itype instr) = getRd instr
instance HasDestination Utype where
  destination (Utype instr) = getRd instr
instance HasDestination Jtype where
  destination (Jtype instr) = getRd instr

data BOpTag = BEQ | BNE | BLT | BGE | BLTU | BGEU deriving(Enum, Bounded, Eq)
instance Represent BOpTag where
  type EnumW BOpTag = 3

type BOp = EnumBit BOpTag

fshowBOp :: BOp -> Format
fshowBOp bop =
  formatCond (bop === enum BEQ) (fshow "beq") <>
  formatCond (bop === enum BNE) (fshow "bne") <>
  formatCond (bop === enum BLT) (fshow "blt") <>
  formatCond (bop === enum BGE) (fshow "bge") <>
  formatCond (bop === enum BLTU) (fshow "bltu") <>
  formatCond (bop === enum BGEU) (fshow "bgeu")

decodeBtype :: Btype -> Option BOp
decodeBtype instr =
  listIndex (function3 instr) [
    some (enum BEQ), -- 000
    some (enum BNE), -- 001
    none,            -- 010
    none,            -- 011
    some (enum BLT), -- 100
    some (enum BGE), -- 101
    some (enum BLTU),-- 110
    some (enum BGEU) -- 111
  ]

data ROpTag
  = ADD
  | SUB
  | SLL
  | SLT
  | SLTU
  | XOR
  | SRL
  | SRA
  | OR
  | AND
  | MUL
  | MULH
  | MULHSU
  | MULHU
  | DIV
  | DIVU
  | REM
  | REMU
  deriving(Bounded, Enum)

instance Represent ROpTag where
  type EnumW ROpTag = 5

type ROp = EnumBit ROpTag

fshowROp :: ROp -> Format
fshowROp rop =
  formatCond (rop === enum ADD) (fshow "add") <>
  formatCond (rop === enum SUB) (fshow "sub") <>
  formatCond (rop === enum SLL) (fshow "sll") <>
  formatCond (rop === enum SLT) (fshow "slt") <>
  formatCond (rop === enum SLTU) (fshow "sltu") <>
  formatCond (rop === enum XOR) (fshow "xor") <>
  formatCond (rop === enum SRL) (fshow "srl") <>
  formatCond (rop === enum SRA) (fshow "sra") <>
  formatCond (rop === enum OR) (fshow "or") <>
  formatCond (rop === enum AND) (fshow "and") <>
  formatCond (rop === enum MUL) (fshow "mul") <>
  formatCond (rop === enum MULH) (fshow "mulh") <>
  formatCond (rop === enum MULHSU) (fshow "mulhsu") <>
  formatCond (rop === enum MULHU) (fshow "mulhu") <>
  formatCond (rop === enum DIV) (fshow "div") <>
  formatCond (rop === enum DIVU) (fshow "divu") <>
  formatCond (rop === enum REM) (fshow "rem") <>
  formatCond (rop === enum REMU) (fshow "remu")

decodeRtype :: Rtype -> Option ROp
decodeRtype instr =
  selectDefault none [
    (f7 === 0b0000000 .&&. f3 === 0b000, some (enum ADD)),
    (f7 === 0b0100000 .&&. f3 === 0b000, some (enum SUB)),
    (f7 === 0b0000000 .&&. f3 === 0b001, some (enum SLL)),
    (f7 === 0b0000000 .&&. f3 === 0b010, some (enum SLT)),
    (f7 === 0b0000000 .&&. f3 === 0b011, some (enum SLTU)),
    (f7 === 0b0000000 .&&. f3 === 0b100, some (enum XOR)),
    (f7 === 0b0000000 .&&. f3 === 0b101, some (enum SRL)),
    (f7 === 0b0100000 .&&. f3 === 0b101, some (enum SRA)),
    (f7 === 0b0000000 .&&. f3 === 0b110, some (enum OR)),
    (f7 === 0b0000000 .&&. f3 === 0b111, some (enum AND)),

    (f7 === 0b0000001 .&&. f3 === 0b000, some (enum MUL)),
    (f7 === 0b0000001 .&&. f3 === 0b001, some (enum MULH)),
    (f7 === 0b0000001 .&&. f3 === 0b010, some (enum MULHSU)),
    (f7 === 0b0000001 .&&. f3 === 0b011, some (enum MULHU)),
    (f7 === 0b0000001 .&&. f3 === 0b100, some (enum DIV)),
    (f7 === 0b0000001 .&&. f3 === 0b101, some (enum DIVU)),
    (f7 === 0b0000001 .&&. f3 === 0b110, some (enum REM)),
    (f7 === 0b0000001 .&&. f3 === 0b111, some (enum REMU))
  ]
  where
    f7 = function7 instr
    f3 = function3 instr

data UOpTag
  = AUIPC
  | LUI
  deriving(Enum, Bounded)

instance Represent UOpTag where
  type EnumW UOpTag = 1

type UOp = EnumBit UOpTag

fshowUOp :: UOp -> Format
fshowUOp uop =
  formatCond (uop === enum AUIPC) (fshow "auipc") <>
  formatCond (uop === enum LUI) (fshow "lui")

decodeUtype :: Utype -> UOp
decodeUtype instr = opcode instr === 0b0110111 ? (enum LUI, enum AUIPC)

data SOpTag
  = SW
  | SB
  | SH
  deriving(Enum, Bounded)

instance Represent SOpTag where
  type EnumW SOpTag = 2
type SOp = EnumBit SOpTag

fshowSOp :: SOp -> Format
fshowSOp sop =
  formatCond (sop === enum SW) (fshow "sw") <>
  formatCond (sop === enum SH) (fshow "sh") <>
  formatCond (sop === enum SB) (fshow "sb")

decodeStype :: Stype -> Option SOp
decodeStype instr =
  selectDefault none [
    (function3 instr === 0b000, some (enum SB)),
    (function3 instr === 0b001, some (enum SH)),
    (function3 instr === 0b010, some (enum SW))
  ]


data IOpTag
  = JALR
  | LW
  | LB
  | LH
  | LBU
  | LHU
  | ADDI
  | SLTI
  | SLTIU
  | XORI
  | ORI
  | ANDI
  | SLLI
  | SRLI
  | SRAI
  | FENCE
  | FENCEI
  | ECALL
  | CSRRW
  | CSRRS
  | CSRRC
  | CSRRWI
  | CSRRSI
  | CSRRCI
  | MRET
  deriving(Bounded, Enum)

instance Represent IOpTag where
  type EnumW IOpTag = 5
type IOp = EnumBit IOpTag

fshowIOp :: IOp -> Format
fshowIOp iop =
  formatCond (iop === enum JALR) (fshow "jalr") <>
  formatCond (iop === enum LW) (fshow "lw") <>
  formatCond (iop === enum LB) (fshow "lb") <>
  formatCond (iop === enum LH) (fshow "lh") <>
  formatCond (iop === enum LBU) (fshow "lbu") <>
  formatCond (iop === enum LHU) (fshow "lhu") <>
  formatCond (iop === enum ADDI) (fshow "addi") <>
  formatCond (iop === enum SLTI) (fshow "slti") <>
  formatCond (iop === enum SLTIU) (fshow "sltiu") <>
  formatCond (iop === enum XORI) (fshow "xori") <>
  formatCond (iop === enum ORI) (fshow "ori") <>
  formatCond (iop === enum ANDI) (fshow "andi") <>
  formatCond (iop === enum SLLI) (fshow "slli") <>
  formatCond (iop === enum SRLI) (fshow "srli") <>
  formatCond (iop === enum SRAI) (fshow "srai") <>
  formatCond (iop === enum FENCE) (fshow "fence") <>
  formatCond (iop === enum FENCEI) (fshow "fencei ") <>
  formatCond (iop === enum ECALL) (fshow "ecall") <>
  formatCond (iop === enum CSRRW) (fshow "csrrw") <>
  formatCond (iop === enum CSRRS) (fshow "csrrs") <>
  formatCond (iop === enum CSRRC) (fshow "csrrc") <>
  formatCond (iop === enum CSRRWI) (fshow "csrrwi") <>
  formatCond (iop === enum CSRRSI) (fshow "csrrsi") <>
  formatCond (iop === enum CSRRCI) (fshow "csrrci") <>
  formatCond (iop === enum MRET) (fshow "mret")

decodeItype :: Itype -> Option IOp
decodeItype instr =
  selectDefault none [
    (op === 0b1100111 .&&. f3 === 0b000, some (enum JALR)),

    (op === 0b0000011 .&&. f3 === 0b000, some (enum LB)),
    (op === 0b0000011 .&&. f3 === 0b001, some (enum LH)),
    (op === 0b0000011 .&&. f3 === 0b010, some (enum LW)),
    (op === 0b0000011 .&&. f3 === 0b100, some (enum LBU)),
    (op === 0b0000011 .&&. f3 === 0b101, some (enum LHU)),

    (op === 0b0010011 .&&. f3 === 0b000, some (enum ADDI)),
    (op === 0b0010011 .&&. f3 === 0b010, some (enum SLTI)),
    (op === 0b0010011 .&&. f3 === 0b011, some (enum SLTIU)),
    (op === 0b0010011 .&&. f3 === 0b100, some (enum XORI)),
    (op === 0b0010011 .&&. f3 === 0b110, some (enum ORI)),
    (op === 0b0010011 .&&. f3 === 0b111, some (enum ANDI)),
    (op === 0b0010011 .&&. f3 === 0b001 .&&. slice @11 @5 imm === 0, some (enum SLLI)),
    (op === 0b0010011 .&&. f3 === 0b101 .&&. slice @11 @5 imm === 0, some (enum SRLI)),
    (op === 0b0010011 .&&. f3 === 0b101 .&&. slice @11 @5 imm === 0b0100000, some (enum SRAI)),

    (isFence .&&. f3 === 0b000, some (enum FENCE)),
    (isFence .&&. f3 === 0b001, some (enum FENCEI)),
    (isECall, some (enum ECALL)),
    (isMRet, some (enum MRET)),

    (op === 0b1110011 .&&. f3 === 0b001, some (enum CSRRW)),
    (op === 0b1110011 .&&. f3 === 0b010, some (enum CSRRS)),
    (op === 0b1110011 .&&. f3 === 0b011, some (enum CSRRC)),
    (op === 0b1110011 .&&. f3 === 0b101, some (enum CSRRWI)),
    (op === 0b1110011 .&&. f3 === 0b110, some (enum CSRRSI)),
    (op === 0b1110011 .&&. f3 === 0b111, some (enum CSRRCI))
  ]
  where
    op = opcode instr
    f3 = function3 instr
    imm = immediate instr
    rs1 = register1 instr
    rd = destination instr

    isFence = op === 0b0001111 .&&. slice @11 @8 imm === 0 .&&.
      rs1 === ArchReg 0 .&&. rd === ArchReg 0

    isMRet = op === 0b1110011 .&&. f3 === 0 .&&. rs1 === ArchReg 0 .&&.
      rd === ArchReg 0 .&&. imm === 0b001100000010

    isECall = op === 0b1110011 .&&. f3 === 0 .&&. rs1 === ArchReg 0 .&&.
      rd === ArchReg 0 .&&. imm === 0

type InstrOp =
  TaggedUnion [
    "jtype" ::: Bit 0,
    "itype" ::: IOp,
    "rtype" ::: ROp,
    "btype" ::: BOp,
    "stype" ::: SOp,
    "utype" ::: UOp
  ]

fshowInstrOp :: InstrOp -> Format
fshowInstrOp instr =
  formatCond (instr `is` #jtype) (fshow "jal") <>
  formatCond (instr `is` #itype) (fshowIOp (untag #itype instr)) <>
  formatCond (instr `is` #rtype) (fshowROp (untag #rtype instr)) <>
  formatCond (instr `is` #btype) (fshowBOp (untag #btype instr)) <>
  formatCond (instr `is` #stype) (fshowSOp (untag #stype instr)) <>
  formatCond (instr `is` #utype) (fshowUOp (untag #utype instr))

data Instr = Instr {op :: InstrOp, bits :: Bit 32} deriving(Generic, Bits)

instance HasOpcode Instr where
  opcode instr = getOpcode instr.bits

instance HasImmediate Instr where
  immediate instr =
    selectDefault 0 [
      (instr.op `is` #itype, immediate (Itype instr.bits)),
      (instr.op `is` #stype, immediate (Stype instr.bits)),
      (instr.op `is` #btype, immediate (Btype instr.bits)),
      (instr.op `is` #utype, immediate (Utype instr.bits)),
      (instr.op `is` #jtype, immediate (Jtype instr.bits))
    ]

instance HasRegister1 Instr where
  register1 instr =
    selectDefault (ArchReg 0) [
      (instr.op `is` #rtype, register1 $ Rtype instr.bits),
      (instr.op `is` #stype, register1 $ Stype instr.bits),
      (instr.op `is` #btype, register1 $ Btype instr.bits),
      (instr.op `is` #itype .&&. inv noRs1, register1 $ Itype instr.bits)
    ]
      where
        iop = untag #itype instr.op
        noRs1 = iop === enum FENCE .||. iop === enum ECALL .||.
          iop === enum CSRRWI .||. iop === enum CSRRCI .||.
            iop === enum CSRRSI .||. iop === enum MRET .||.
              iop === enum FENCEI

instance HasRegister2 Instr where
  register2 instr =
    selectDefault (ArchReg 0) [
      (instr.op `is` #rtype, register2 $ Rtype instr.bits),
      (instr.op `is` #stype, register2 $ Stype instr.bits),
      (instr.op `is` #btype, register2 $ Btype instr.bits)
    ]

instance HasDestination Instr where
  destination instr =
    selectDefault (ArchReg 0) [
      (instr.op `is` #rtype, destination $ Rtype instr.bits),
      (instr.op `is` #utype, destination $ Utype instr.bits),
      (instr.op `is` #jtype, destination $ Jtype instr.bits),
      (instr.op `is` #itype .&&. inv noRd, destination $ Itype instr.bits)
    ]
      where
        iop = untag #itype instr.op
        noRd = iop === enum FENCE .||. iop === enum ECALL .||.
          iop === enum FENCEI .||. iop === enum MRET

decodeInstr :: Bit 32 -> Option Instr
decodeInstr bits =
  selectDefault none [
    (optype === some itype .&&. iop =!= none, some Instr{op= tag #itype iop.val, bits}),
    (optype === some rtype .&&. rop =!= none, some Instr{op= tag #rtype rop.val, bits}),
    (optype === some btype .&&. bop =!= none, some Instr{op= tag #btype bop.val, bits}),
    (optype === some stype .&&. sop =!= none, some Instr{op= tag #stype sop.val, bits}),
    (optype === some utype, some Instr{op= tag #utype uop, bits}),
    (optype === some jtype, some Instr{op= tag #jtype 0, bits})
  ]
  where
    optype = opcodeType (getOpcode bits)
    iop = decodeItype (Itype bits)
    rop = decodeRtype (Rtype bits)
    sop = decodeStype (Stype bits)
    uop = decodeUtype (Utype bits)
    bop = decodeBtype (Btype bits)

fshowInstr :: Instr -> Format
fshowInstr instr =
  formatCond (instr.op `is` #itype) ifshow <>
  formatCond (instr.op `is` #jtype) jfshow <>
  formatCond (instr.op `is` #rtype) rfshow <>
  formatCond (instr.op `is` #btype) bfshow <>
  formatCond (instr.op `is` #stype) sfshow <>
  formatCond (instr.op `is` #utype) ufshow
    where
      iop = fshowIOp (untag #itype instr.op)
      rop = fshowROp (untag #rtype instr.op)
      sop = fshowSOp (untag #stype instr.op)
      bop = fshowBOp (untag #btype instr.op)
      uop = fshowUOp (untag #utype instr.op)

      imm = toSigned (immediate instr)
      immHex = fshow "0x" <> formatHex 0 (immediate instr)
      immDec =
        formatCond (imm .>=. zero) (formatDec 0 (immediate instr)) <>
        formatCond (imm .<. zero) (fshow "-" <> formatDec 0 (0 .-. immediate instr))
      rs1 = fshow (register1 instr)
      rs2 = fshow (register2 instr)
      rd = fshow (destination instr)

      ifshow = iop <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> immDec
      jfshow = fshow "jal " <> immHex
      rfshow = rop <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> rs2
      bfshow = bop <> fshow " " <> rs1 <> fshow ", " <> rs2 <> fshow ", " <> immDec
      sfshow = sop <> fshow " " <> rs1 <> fshow ", " <> rs2 <> fshow ", " <> immDec
      ufshow = uop <> fshow " " <> rd <> fshow ", " <> immHex
