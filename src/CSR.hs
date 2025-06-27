module CSR where

import Blarney
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

makeCycleCounterCSR :: Module [CSR]
makeCycleCounterCSR = do
  counter :: Reg (Bit 64) <- makeReg 0

  always do
    counter <== counter.val + 1

  return
    [ CSR
        { csrId= 0xb00
        , csrRead= Just (return (truncate counter.val))
        , csrWrite= Nothing }
    , CSR
        { csrId= 0xb80
        , csrRead= Just (return (truncateLSB counter.val))
        , csrWrite= Nothing }]

makeInstructionCounterCSR :: Module ([CSR], Action ())
makeInstructionCounterCSR = do
  counter :: Reg (Bit 64) <- makeReg 0

  return
    ( [ CSR
          { csrId= 0xb02
          , csrRead= Just (return (truncate counter.val))
          , csrWrite= Nothing }
      , CSR
          { csrId= 0xb82
          , csrRead= Just (return (truncateLSB counter.val))
          , csrWrite= Nothing }]
    , (counter <== counter.val + 1))

makeHartIdCSR :: Bit 32 -> Module [CSR]
makeHartIdCSR id = do
  return
    [ CSR
        { csrId= 0xf14
        , csrRead= Just (return id)
        , csrWrite= Nothing }]
