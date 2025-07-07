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

  -- Wait for a given delay in nanosecond
  let until :: Integer -> Stmt () = \ x -> do
        action do
          counter <== lit (div (x * clkMhz) 1000)
        while (counter.val =!= 0) do
          action do
            counter <== counter.val - 1

  let sendCmd :: Bit 8 -> Stmt () = \ x -> do
        wait commandSink.canPut
        action do
          commandSink.put x
        until 10_000

  let sendDat :: Bit 8 -> Stmt () = \ x -> do
        wait dataSink.canPut
        action do
          dataSink.put x
        until 10_000

  runStmt do
    until 50
    action do
      vdd <== 0
    until 1_000_000
    sendCmd 0xAE -- DisplayOff
    action do
      reset <== 0
    until 1_000_000
    action do
      reset <== 1
    until 1_000_000

    sendCmd 0x8D -- ChargePump1
    sendCmd 0x14 -- ChargePump2
    sendCmd 0xD9 -- PreCharge1
    sendCmd 0xF1 -- PreCharge2

    action do
      vbat <== 0
    until 1_000_000

    sendCmd 0x81 -- DispContrast1
    sendCmd 0x0F -- DispContrast2
    sendCmd 0xA0 -- SetSegRemap
    sendCmd 0xC0 -- SetScanDirection
    sendCmd 0xDA -- SetLowerColumnAddress
    sendCmd 0x00 -- LowerColumnAddress
    sendCmd 0xAF -- DisplayOn

    until 1_000_000

    sendCmd 0xA5

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
