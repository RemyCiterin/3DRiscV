module CSR where

import Blarney
import Blarney.Utils
import Instr

--Reminder:
--    | ustatus          | 000 |
--    | uie              | 004 |
--    | utvec            | 005 |
--    | uscratch         | 040 |
--    | uepc             | 041 |
--    | ucause           | 042 |
--    | ubadaddr         | 043 |
--    | uip              | 044 |
--    | fflags           | 001 |
--    | frm              | 002 |
--    | fcsr             | 003 |
--    | cycle            | c00 |
--    | time             | c01 |
--    | instret          | c02 |
--    | cycleh           | c80 |
--    | timeh            | c81 |
--    | instreth         | c82 |
--    | sstatus          | 100 |
--    | sedeleg          | 102 |
--    | sideleg          | 103 |
--    | sie              | 104 |
--    | stvec            | 105 |
--    | sscratch         | 140 |
--    | sepc             | 141 |
--    | scause           | 142 |
--    | sbadaddr         | 143 |
--    | sip              | 144 |
--    | sptbr            | 180 |
--    | scycle           | d00 |
--    | stime            | d01 |
--    | sinstret         | d02 |
--    | scycleh          | d80 |
--    | stimeh           | d81 |
--    | sinstreth        | d82 |
--    | hstatus          | 200 |
--    | hedeleg          | 202 |
--    | hideleg          | 203 |
--    | hie              | 204 |
--    | htvec            | 205 |
--    | hscratch         | 240 |
--    | hepc             | 241 |
--    | hcause           | 242 |
--    | hbadaddr         | 243 |
--    | hcycle           | e00 |
--    | htime            | e01 |
--    | hinstret         | e02 |
--    | hcycleh          | e80 |
--    | htimeh           | e81 |
--    | hinstreth        | e82 |
--    | misa             | f10 |
--    | mvendorid        | f11 |
--    | marchid          | f12 |
--    | mimpid           | f13 |
--    | mhartid          | f14 |
--    | mstatus          | 300 |
--    | medeleg          | 302 |
--    | mideleg          | 303 |
--    | mie              | 304 |
--    | mtvec            | 305 |
--    | mscratch         | 340 |
--    | mepc             | 341 |
--    | mcause           | 342 |
--    | mtval            | 343 |
--    | mip              | 344 |
--    | mbase            | 380 |
--    | mbound           | 381 |
--    | mibase           | 382 |
--    | mibound          | 383 |
--    | mdbase           | 384 |
--    | mdbound          | 385 |
--    | mcycle           | b00 |
--    | mtime            | f01 |
--    | minstret         | b02 |
--    | mcycleh          | b80 |
--    | mtimeh           | f81 |
--    | minstreth        | b82 |
--    | mucounteren      | 310 |
--    | mscounteren      | 311 |
--    | mhcounteren      | 312 |
--    | mucycle_delta    | 700 |
--    | mutime_delta     | 701 |
--    | muinstret_delta  | 702 |
--    | mscycle_delta    | 704 |
--    | mstime_delta     | 705 |
--    | msinstret_delta  | 706 |
--    | mhcycle_delta    | 708 |
--    | mhtime_delta     | 709 |
--    | mhinstret_delta  | 70a |
--    | mucycle_deltah   | 780 |
--    | mutime_deltah    | 781 |
--    | muinstret_deltah | 782 |
--    | mscycle_deltah   | 784 |
--    | mstime_deltah    | 785 |
--    | msinstret_deltah | 786 |
--    | mhcycle_deltah   | 788 |
--    | mhtime_deltah    | 789 |
--    | mhinstret_deltah | 78a |

data Priv =
  Priv (Bit 2)
  deriving(Bits, Generic, Cmp)

user_priv = Priv 0
supervisor_priv = Priv 1
machine_priv = Priv 3

minPrivCSR :: Bit 12 -> Priv
minPrivCSR id = Priv (slice @9 @8 id)

isReadOnlyCSR :: Bit 12 -> Bit 1
isReadOnlyCSR id = slice @11 @10 id === 0b11

data CSR =
  CSR
    { csrId :: Bit 12
    , csrRead :: Maybe (Action (Bit 32))
    , csrWrite :: Maybe (Bit 32 -> Action ()) }

data CSRUnit =
  CSRUnit
    { csrUnitRead :: Bit 12 -> Action (Bit 32)
    , csrUnitWrite :: Bit 12 -> Bit 32 -> Action () }

makeCSRUnit :: [CSR] -> Module CSRUnit
makeCSRUnit csrs = do
  rdId :: Wire (Bit 12) <- makeWire dontCare
  wrId :: Wire (Bit 12) <- makeWire dontCare

  rdVal :: Wire (Bit 32) <- makeWire dontCare
  wrVal :: Wire (Bit 32) <- makeWire dontCare

  always do
    forM_ csrs \ csr -> do
      case csr.csrWrite of
        Nothing -> return ()
        Just write -> do
          when (wrId.active .&&. wrId.val === csr.csrId) do
            write wrVal.val

      case csr.csrRead of
        Nothing -> pure ()
        Just read -> do
          when (rdId.active .&&. rdId.val === csr.csrId) do
            x <- read
            rdVal <== x

  return
    CSRUnit
      { csrUnitRead= \ id -> do
          rdId <== id
          return rdVal.val
      , csrUnitWrite= \ id value -> do
          --display "write csr " (formatHex 0 id) " := " (formatHex 0 value)
          wrVal <== value
          wrId <== id }

nullCSRUnit :: CSRUnit
nullCSRUnit =
  CSRUnit
    { csrUnitRead= \ _ -> return 0
    , csrUnitWrite= \ _ _ -> return () }

readOnlyCSR :: Bit 12 -> Bit 32 -> CSR
readOnlyCSR id value =
  CSR
    { csrId= id
    , csrRead= Just (return value)
    , csrWrite= Nothing }

writeOnlyCSR :: Bit 12 -> (Bit 32 -> Action ()) -> CSR
writeOnlyCSR id upd =
  CSR
    { csrId= id
    , csrRead= Nothing
    , csrWrite= Just upd }

readWriteCSR :: Bit 12 -> Bit 32 -> (Bit 32 -> Action ()) -> CSR
readWriteCSR id value upd =
  CSR
    { csrId= id
    , csrRead= Just (return value)
    , csrWrite= Just upd }

regToCSR :: Bit 12 -> Reg (Bit 32) -> CSR
regToCSR id reg = readWriteCSR id reg.val reg.writeReg

makeCycleCounterCSR :: Module [CSR]
makeCycleCounterCSR = do
  counter :: Reg (Bit 64) <- makeReg 0

  always do
    counter <== counter.val + 1

  return
    [ readOnlyCSR 0xb00 (truncate counter.val)
    , readOnlyCSR 0xb80 (truncateLSB counter.val) ]

makeInstructionCounterCSR :: Module ([CSR], Action ())
makeInstructionCounterCSR = do
  counter :: Reg (Bit 64) <- makeReg 0

  return
    ( [ readOnlyCSR 0xb02 (truncate counter.val)
      , readOnlyCSR 0xb82 (truncateLSB counter.val) ]
    , (counter <== counter.val + 1))

makeHartIdCSR :: Bit 32 -> Module [CSR]
makeHartIdCSR id = do
  return [readOnlyCSR 0xf14 id]

data TrapCSRs =
  TrapCSRs
    { mepc :: Reg (Bit 32)
    , mcause :: Reg (Bit 32)
    , mtvec :: Reg (Bit 32)
    , mtval :: Reg (Bit 32) }

makeTrapCSRs :: Module ([CSR], TrapCSRs)
makeTrapCSRs = do
  mepc :: Reg (Bit 32) <- makeReg dontCare
  mcause :: Reg (Bit 32) <- makeReg dontCare
  mtvec :: Reg (Bit 32) <- makeReg dontCare
  mtval :: Reg (Bit 32) <- makeReg dontCare

  let csrs =
        [ regToCSR 0x341 mepc
        , regToCSR 0x305 mtvec
        , readOnlyCSR 0x343 mtval.val
        , readOnlyCSR 0x342 mcause.val ]

  let regs =
        TrapCSRs
          { mepc
          , mcause
          , mtvec
          , mtval }

  return (csrs, regs)

data MieCSRs =
  MieCSRs
    { meie :: Reg (Bit 1)
    , mtie :: Reg (Bit 1)
    , msie :: Reg (Bit 1)
    , all :: Bit 32 }

makeMieCSRs :: Module ([CSR], MieCSRs)
makeMieCSRs = do
  meie :: Reg (Bit 1) <- makeReg false
  mtie :: Reg (Bit 1) <- makeReg false
  msie :: Reg (Bit 1) <- makeReg false

  let mie :: Reg (Bit 32) =
        readOnlyReg @20 0
        `concatReg` meie
        `concatReg` readOnlyReg @3 0
        `concatReg` mtie
        `concatReg` readOnlyReg @3 0
        `concatReg` msie
        `concatReg` readOnlyReg @3 0

  return ([regToCSR 0x304 mie], MieCSRs{meie, mtie, msie, all= mie.val})

data MipCSRs =
  MipCSRs
    { meip :: Reg (Bit 1)
    , mtip :: Reg (Bit 1)
    , msip :: Reg (Bit 1)
    , all :: Bit 32 }

makeMipCSRs :: Module ([CSR], MipCSRs)
makeMipCSRs = do
  meip :: Reg (Bit 1) <- makeReg false
  mtip :: Reg (Bit 1) <- makeReg false
  msip :: Reg (Bit 1) <- makeReg false

  let mip :: Reg (Bit 32) =
        readOnlyReg @20 0
        `concatReg` meip
        `concatReg` readOnlyReg @3 0
        `concatReg` mtip
        `concatReg` readOnlyReg @3 0
        `concatReg` msip
        `concatReg` readOnlyReg @3 0

  return ([readOnlyCSR 0x344 mip.val], MipCSRs{meip, mtip, msip, all= mip.val})

data MstatusCSRs=
  MstatusCSRs
    { mie :: Reg (Bit 1)
    , mpie :: Reg (Bit 1) }

makeMstatusCSRs :: Module ([CSR], MstatusCSRs)
makeMstatusCSRs = do
  mie :: Reg (Bit 1) <- makeReg false
  mpie :: Reg (Bit 1) <- makeReg false

  let mstatus :: Reg (Bit 32) =
        readOnlyReg @24 0
        `concatReg` mpie
        `concatReg` readOnlyReg @3 0
        `concatReg` mie
        `concatReg` readOnlyReg @3 0

  return ([regToCSR 0x300 mstatus], MstatusCSRs{mie, mpie})

makeMscratchCSRs :: Module [CSR]
makeMscratchCSRs = do
  mscratch :: Reg (Bit 32) <- makeReg dontCare

  return [regToCSR 0x340 mscratch]
