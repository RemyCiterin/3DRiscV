module CSR where

import Blarney
import Utils
import Instr

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
        , readOnlyCSR 0x342 mcause.val]

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
