module Instr where

import Data.Char

import Blarney
import Blarney.BitScan
import Blarney.Option
import Blarney.ADT

import Utils

type RegId = Bit 5

fshowRegId :: RegId -> Format
fshowRegId reg =
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

data Mnemonic =
    LUI
  | AUIPC
  | ADD
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SRA
  | SUB
  | JAL
  | JALR
  | BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  | LOAD
  | STORE
  | FENCE
  | ECALL
  | EBREAK
  | CSRRW
  | CSRRS
  | CSRRC
  | LOADR
  | STOREC
  | AMOAND
  | AMOADD
  | AMOOR
  | AMOXOR
  | AMOMIN
  | AMOMAX
  | AMOMINU
  | AMOMAXU
  | AMOSWAP
  deriving (Bounded, Enum, Show, Ord, Eq)

-- | Upper bound on number of instruction mnemonics used by the decoder
type MaxMnemonics = 64

-- | Bit vector indentifying one or more active mnemonics
type MnemonicVec = Bit MaxMnemonics

--  -- Checking if any of the given mnemonics are active
--  infix 8 `is`
--  is :: MnemonicVec -> [Mnemonic] -> Bit 1
--  is vec ms = orList [unsafeAt (fromEnum m) vec | m <- ms]

instance HasIs MnemonicVec [Mnemonic] where
  is vec ms = orList [unsafeAt (fromEnum m) vec | m <- ms]

-- RV32I instruction decode table
-- ==============================

decodeTable =
  [ "imm[31:12] rd<5> 0110111" --> LUI
  , "imm[31:12] rd<5> 0010111" --> AUIPC
  , "imm[11:0] rs1<5> 000 rd<5> 0010011" --> ADD
  , "imm[11:0] rs1<5> 010 rd<5> 0010011" --> SLT
  , "imm[11:0] rs1<5> 011 rd<5> 0010011" --> SLTU
  , "imm[11:0] rs1<5> 111 rd<5> 0010011" --> AND
  , "imm[11:0] rs1<5> 110 rd<5> 0010011" --> OR
  , "imm[11:0] rs1<5> 100 rd<5> 0010011" --> XOR
  , "0000000 imm[4:0] rs1<5> 001 rd<5> 0010011" --> SLL
  , "0000000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRL
  , "0100000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRA
  , "0000000 rs2<5> rs1<5> 000 rd<5> 0110011" --> ADD
  , "0000000 rs2<5> rs1<5> 010 rd<5> 0110011" --> SLT
  , "0000000 rs2<5> rs1<5> 011 rd<5> 0110011" --> SLTU
  , "0000000 rs2<5> rs1<5> 111 rd<5> 0110011" --> AND
  , "0000000 rs2<5> rs1<5> 110 rd<5> 0110011" --> OR
  , "0000000 rs2<5> rs1<5> 100 rd<5> 0110011" --> XOR
  , "0100000 rs2<5> rs1<5> 000 rd<5> 0110011" --> SUB
  , "0000000 rs2<5> rs1<5> 001 rd<5> 0110011" --> SLL
  , "0000000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRL
  , "0100000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRA
  , "imm[20] imm[10:1] imm[11] imm[19:12] rd<5> 1101111" --> JAL
  , "imm[11:0] rs1<5> 000 rd<5> 1100111" --> JALR
  , "off[12] off[10:5] rs2<5> rs1<5> 000 off[4:1] off[11] 1100011" --> BEQ
  , "off[12] off[10:5] rs2<5> rs1<5> 001 off[4:1] off[11] 1100011" --> BNE
  , "off[12] off[10:5] rs2<5> rs1<5> 100 off[4:1] off[11] 1100011" --> BLT
  , "off[12] off[10:5] rs2<5> rs1<5> 110 off[4:1] off[11] 1100011" --> BLTU
  , "off[12] off[10:5] rs2<5> rs1<5> 101 off[4:1] off[11] 1100011" --> BGE
  , "off[12] off[10:5] rs2<5> rs1<5> 111 off[4:1] off[11] 1100011" --> BGEU
  , "000000000000 <5> 000 <5> 1110011" --> ECALL
  , "000000000001 <5> 000 <5> 1110011" --> EBREAK
  , "fence<4> pred<4> succ<4> rs1<5> 000 00000 0001111" --> FENCE
  , "csr[11:0] rs1<5> csrI<1> 01 rd<5> 1110011" --> CSRRW
  , "csr[11:0] rs1<5> csrI<1> 10 rd<5> 1110011" --> CSRRS
  , "csr[11:0] rs1<5> csrI<1> 11 rd<5> 1110011" --> CSRRC
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> LOAD
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> STORE
  , "00010 aq<1> rl<1> 00000 rs1<5> 010 rd<5> 0101111" --> LOADR
  , "00011 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> STOREC
  , "00001 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOSWAP
  , "00000 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOADD
  , "00100 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOXOR
  , "01100 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOAND
  , "01000 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOOR
  , "10000 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOMIN
  , "10100 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOMAX
  , "11000 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOMINU
  , "11100 aq<1> rl<1> rs2<5> rs1<5> 010 rd<5> 0101111" --> AMOMAXU
  ]

data Instr =
  Instr {
    rd          :: Option RegId
  , rs1         :: Option RegId
  , rs2         :: Option RegId
  , imm         :: Option (Bit 32)
  , opcode      :: MnemonicVec
  , off         :: Bit 13
  , csr         :: Bit 12
  , csrI        :: Bit 1
  , accessWidth :: Bit 2
  , isUnsigned  :: Bit 1
  , isMemAccess :: Bit 1
  , canBranch   :: Bit 1
  , isAcquire   :: Bit 1
  , isRelease   :: Bit 1
  , isAMO       :: Bit 1
  , pred        :: Bit 4
  , succ        :: Bit 4
  } deriving (Generic, Bits)

decodeInstr :: Bit 32 -> Instr
decodeInstr instr =
  Instr {
    rd = Option (hasBitField fieldMap "rd" .&&. regDest =!= 0) regDest
  , rs1 = Option (hasBitField fieldMap "rs1") (getBitFieldSel selMap "rs1" instr)
  , rs2 = Option (hasBitField fieldMap "rs2") (getBitFieldSel selMap "rs2" instr)
  , imm = getBitField fieldMap "imm"
  , opcode = opcode
  , off = getBitFieldSel selMap "off" instr
  , csr = getBitFieldSel selMap "csr" instr
  , csrI = getBitFieldSel selMap "csrI" instr
  , accessWidth = getBitFieldSel selMap "aw" instr
  , isUnsigned = getBitFieldSel selMap "ul" instr
  , isMemAccess = opcode `is` [LOAD,STORE,FENCE,STOREC,LOADR] .||. isAMO
  , canBranch = opcode `is` [JAL,JALR,BEQ,BNE,BLT,BLTU,BGE,BGEU]
  , isAcquire = getBitFieldSel selMap "aq" instr
  , isRelease = getBitFieldSel selMap "rl" instr
  , pred = getBitFieldSel selMap "pred" instr
  , succ = getBitFieldSel selMap "succ" instr
  , isAMO
  }
  where
    regDest = getBitFieldSel selMap "rd" instr :: RegId
    (tagMap, fieldMap) = matchMap False decodeTable instr
    isAMO = opcode `is` [AMOOR,AMOAND,AMOXOR,AMOSWAP,AMOADD,AMOMIN,AMOMAX,AMOMINU,AMOMAXU]
    selMap = matchSel decodeTable
    opcode = packTagMap tagMap


-- Instruction pretty printing
instance FShow Instr where
  fshow instr =
    formatCond (instr.opcode === 0) (fshow "invalid-instr") <>
    formatCond (instr.opcode `is` [LUI    ]) (formatUtype      LUI  ) <>
    formatCond (instr.opcode `is` [AUIPC  ]) (formatUtype      AUIPC) <>
    formatCond (instr.opcode `is` [ADD    ]) (formatRItype     ADD  ) <>
    formatCond (instr.opcode `is` [SLT    ]) (formatRItype     SLT  ) <>
    formatCond (instr.opcode `is` [SLTU   ]) (formatRItype     SLTU ) <>
    formatCond (instr.opcode `is` [AND    ]) (formatRItype     AND  ) <>
    formatCond (instr.opcode `is` [OR     ]) (formatRItype     OR   ) <>
    formatCond (instr.opcode `is` [XOR    ]) (formatRItype     XOR  ) <>
    formatCond (instr.opcode `is` [SLL    ]) (formatRItype     SLL  ) <>
    formatCond (instr.opcode `is` [SRL    ]) (formatRItype     SRL  ) <>
    formatCond (instr.opcode `is` [SRA    ]) (formatRItype     SRA  ) <>
    formatCond (instr.opcode `is` [SUB    ]) (formatRtype      SUB  ) <>
    formatCond (instr.opcode `is` [JAL    ]) (formatJtype      JAL  ) <>
    formatCond (instr.opcode `is` [JALR   ]) (formatItype      JALR ) <>
    formatCond (instr.opcode `is` [BEQ    ]) (formatBtype      BEQ  ) <>
    formatCond (instr.opcode `is` [BNE    ]) (formatBtype      BNE  ) <>
    formatCond (instr.opcode `is` [BLT    ]) (formatBtype      BLT  ) <>
    formatCond (instr.opcode `is` [BLTU   ]) (formatBtype      BLTU ) <>
    formatCond (instr.opcode `is` [BGE    ]) (formatBtype      BGE  ) <>
    formatCond (instr.opcode `is` [BGEU   ]) (formatBtype      BGEU ) <>
    formatCond (instr.opcode `is` [LOAD   ]) (formatLtype           ) <>
    formatCond (instr.opcode `is` [STORE  ]) (formatStype           ) <>
    formatCond (instr.opcode `is` [CSRRW  ]) (formatCSRRW      CSRRW) <>
    formatCond (instr.opcode `is` [CSRRS  ]) (formatCSRRS      CSRRS) <>
    formatCond (instr.opcode `is` [CSRRC  ]) (formatCSRRC      CSRRC) <>
    formatCond (instr.opcode `is` [FENCE  ]) (fshow "fence" ) <>
    formatCond (instr.opcode `is` [ECALL  ]) (fshow "ecall" ) <>
    formatCond (instr.opcode `is` [EBREAK ]) (fshow "ebreak") <>
    formatCond (instr.opcode `is` [AMOADD ]) (formatAMO AMOADD ) <>
    formatCond (instr.opcode `is` [AMOOR  ]) (formatAMO AMOOR  ) <>
    formatCond (instr.opcode `is` [AMOSWAP]) (formatAMO AMOSWAP) <>
    formatCond (instr.opcode `is` [AMOAND ]) (formatAMO AMOAND ) <>
    formatCond (instr.opcode `is` [AMOXOR ]) (formatAMO AMOXOR ) <>
    formatCond (instr.opcode `is` [AMOMIN ]) (formatAMO AMOMIN ) <>
    formatCond (instr.opcode `is` [AMOMAX ]) (formatAMO AMOMAX ) <>
    formatCond (instr.opcode `is` [AMOMINU]) (formatAMO AMOMINU) <>
    formatCond (instr.opcode `is` [AMOMAXU]) (formatAMO AMOMAXU) <>
    formatCond (instr.opcode `is` [STOREC ]) (formatAMO STOREC) <>
    formatCond (instr.opcode `is` [LOADR  ]) (formatLR LOADR)
    where
      imm = toSigned instr.imm.val
      immHex = fshow "0x" <> formatHex 0 instr.imm.val
      immDec =
        formatCond (imm .>=. zero) (formatDec 0 instr.imm.val) <>
        formatCond (imm .<. zero) (fshow "-" <> formatDec 0 (-instr.imm.val))

      off = toSigned instr.off
      offHex = fshow "0x" <> formatHex 0 instr.off
      offDec =
        formatCond (off .>=. zero) (formatDec 0 instr.off) <>
        formatCond (off .<. zero) (fshow "-" <> formatDec 0 (-instr.off))

      rs1 = fshowRegId instr.rs1.val
      rs2 = fshowRegId instr.rs2.val
      rd  = fshowRegId instr.rd.val

      formatAcquire = formatCond instr.isAcquire $ fshow ".aq"
      formatRelease = formatCond instr.isRelease $ fshow ".rl"

      formatRItype opcode =
        formatCond (instr.imm.valid) (formatItype opcode) <>
        formatCond (inv instr.imm.valid) (formatRtype opcode)

      csr = fshow "0x" <> formatHex 0 instr.csr

      formatOp opcode = fshow (map toLower (show opcode))
      formatLoad =
        formatCond (instr.accessWidth === 0b00) (fshow "lb") <>
        formatCond (instr.accessWidth === 0b01) (fshow "lh") <>
        formatCond (instr.accessWidth === 0b10) (fshow "lw")
      formatStore =
        formatCond (instr.accessWidth === 0b00) (fshow "sb") <>
        formatCond (instr.accessWidth === 0b01) (fshow "sh") <>
        formatCond (instr.accessWidth === 0b10) (fshow "sw")
      formatItype opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> immDec
      formatRtype opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> rs2
      formatBtype opcode = formatOp opcode <> fshow " " <> rs1 <> fshow ", " <> rs2 <> fshow ", " <> offDec
      formatJtype opcode = formatOp opcode <> immHex
      formatUtype opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> immHex
      formatCSRRW opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> csr
      formatCSRRC opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> csr
      formatCSRRS opcode = formatOp opcode <> fshow " " <> rd <> fshow ", " <> rs1 <> fshow ", " <> csr
      formatStype = formatStore  <> fshow " " <> rs2 <> fshow ", " <> immDec <> fshow "(" <> rs1 <> fshow ")"
      formatLtype = formatLoad <> fshow " " <> rd <> fshow ", " <> immDec <> fshow "(" <> rs1 <> fshow ")"
      formatAMO opcode = formatOp opcode <> formatAcquire <> formatRelease <>
        fshow " " <> rd <> fshow ", " <> rs2 <> fshow ", (" <> rs1 <> fshow ")"
      formatLR opcode = formatOp opcode <> formatAcquire <> formatRelease <>
        fshow " " <> rd <> fshow ", (" <> rs1 <> fshow ")"


data ExecInput = ExecInput
  { instr :: Instr
  , rs1 :: Bit 32
  , rs2 :: Bit 32
  , pc  :: Bit 32}
  deriving(Bits, Generic, FShow)

data ExecOutput = ExecOutput
  { exception :: Bit 1
  , cause :: Bit 4
  , tval :: Bit 32
  , rd :: Bit 32
  , pc :: Bit 32}
  deriving(Bits, Generic, FShow)
