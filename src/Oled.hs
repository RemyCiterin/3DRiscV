module Oled where

import Uart
import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.Stream
import Blarney.Queue
import Blarney.Stmt

data OledFabric =
  OledFabric
    { oled_clk :: Bit 1
    , oled_reset :: Bit 1
    , oled_dout :: Bit 1
    , oled_vdd :: Bit 1
    , oled_vbat :: Bit 1
    , oled_isData :: Bit 1
    , oled_debug :: Bit 8}
    deriving(Generic, Interface)

makeOled :: Integer -> Module OledFabric
makeOled clkMhz = do
  clk :: Reg (Bit 1) <- makeReg false
  phase :: Reg (Bit 16) <- makeReg 9
  divider :: Reg (Bit 16) <- makeReg 9

  counter :: Reg (Bit 64) <- makeReg 0

  vdd :: Reg (Bit 1) <- makeReg 1
  vbat :: Reg (Bit 1) <- makeReg 1
  isData :: Reg (Bit 1) <- makeReg 1
  reset :: Reg (Bit 1) <- makeReg 1

  requestValid :: Reg (Bit 8) <- makeReg 0
  requestBuffer :: Reg (Bit 8) <- makeReg 0

  debug :: Reg (Bit 8) <- makeReg 0

  always do
    when (phase.val =!= 0) do
      phase <== phase.val - 1

    when (phase.val === 0 .&&. requestValid.val =!= 0) do
      when clk.val do
        requestValid <== truncate requestValid.val # (0 :: Bit 1)
        requestBuffer <== truncate requestBuffer.val # (1 :: Bit 1)

      clk <== inv clk.val
      phase <== divider.val

  let sink :: Sink (Bit 8) =
        Sink
          { canPut= requestValid.val === 0 .&&. phase.val === 0
          , put= \ x -> do
              phase <== divider.val
              requestValid <== ones
              requestBuffer <== x }

  let dataSink =
        sink
          {put= \ x -> do
            sink.put x
            isData <== true }
  let commandSink =
        sink
          {put= \ x -> do
            sink.put x
            isData <== false }

  let inc :: Stmt () = action (counter <== counter.val + 1)
  let dec :: Stmt () = action (counter <== counter.val - 1)
  let rst :: Stmt () = action (counter <== 0)

  -- Wait for a given delay in nanosecond
  let until :: Integer -> Stmt () = \ x -> do
        action do
          counter <== lit (div (x * clkMhz) 1000)
        while (counter.val =!= 0) do
          dec

  let sendCmd :: Bit 8 -> Stmt () = \ x -> do
        wait commandSink.canPut
        action do
          commandSink.put x
        --until 10_000

  let sendDat :: Bit 8 -> Stmt () = \ x -> do
        wait dataSink.canPut
        action do
          dataSink.put x
        --until 10_000

  let chargePump1 = 0x8D
  let chargePump2 = 0x14
  let preCharge1 = 0xD9
  let preCharge2 = 0xF1
  let dispContrast1 = 0x81
  let dispContrast2 = 0x0F
  let setSegRemap = 0xA0
  let setScanDirection = 0xC0
  let setLowerColumnAddress = 0xDA
  let lowerColumnAddress = 0x00
  let displayOn = 0xAF
  let displayOff = 0xAE
  let setPage = 0x22
  let leftColumn1 = 0x00
  let leftColumn2 = 0x10

  let updatePageState :: Bit 2 -> Stmt () = \ page -> do
        sendCmd setPage
        sendCmd (zeroExtend page)
        sendCmd leftColumn1
        sendCmd leftColumn2

  runStmt do
    until 50
    action do
      vdd <== 0
    until 1_000_000
    sendCmd displayOff
    action do
      reset <== 0
    until 1_000_000
    action do
      reset <== 1
    until 1_000_000

    sendCmd chargePump1
    sendCmd chargePump2
    sendCmd preCharge1
    sendCmd preCharge2

    action do
      vbat <== 0
    until 1_000_000

    sendCmd dispContrast1
    sendCmd dispContrast2
    sendCmd setSegRemap
    sendCmd setScanDirection
    sendCmd setLowerColumnAddress
    sendCmd lowerColumnAddress
    sendCmd displayOn

    until 1_000_000

    updatePageState 0

    rst
    updatePageState 0
    while (counter.val .<. 16*8) do
      sendDat 0x00
      inc

    rst
    updatePageState 1
    while (counter.val .<. 16*8) do
      sendDat (lower counter.val)
      inc

    rst
    updatePageState 2
    while (counter.val .<. 16*8) do
      sendDat 0x55
      inc

    rst
    updatePageState 3
    while (counter.val .<. 16*8) do
      sendDat 0x55
      inc

    --updatePageState 1
    --action do
    --  counter <== 0
    --while (counter.val .<. 64) do
    --  action do
    --    counter <== counter.val + 1
    --  sendDat 0x30

    --sendCmd 0xA5

    action do
      debug <== ones


  return
    OledFabric
     { oled_clk= clk.val
     , oled_dout= at @7 requestBuffer.val
     , oled_vdd= vdd.val
     , oled_vbat= vbat.val
     , oled_isData= isData.val
     , oled_reset= reset.val
     , oled_debug= debug.val }
