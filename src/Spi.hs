module Spi where

import Uart
import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.Stream
import Blarney.Queue
import Blarney.Stmt

data SpiFabric =
  SpiFabric
    { clk :: Bit 1
    , mosi :: Bit 1
    , miso :: Bit 1 -> Action ()
    , cs :: Bit 1}
    deriving(Generic, Interface)

data SpiIO =
  SpiIO
    { fabric :: SpiFabric
    , setDivider :: Bit 32 -> Action ()
    , setCS :: Bit 1 -> Action () }

makeSpi :: Stream (Bit 8) -> Module (SpiIO, Stream (Bit 8))
makeSpi inputs = do
  divider :: Reg (Bit 32) <- makeReg 0

  phase :: Reg (Bit 32) <- makeReg 0

  cs :: Reg (Bit 1) <- makeReg 0

  clk :: Reg (Bit 1) <- makeReg 0

  responseBuffer :: Reg (Bit 8) <- makeReg 0
  responseValid :: Reg (Bit 8) <- makeReg 0

  requestBuffer :: Reg (Bit 8) <- makeReg 0
  requestValid :: Reg (Bit 8) <- makeReg 0

  let mosi :: Bit 1 = truncateLSB requestBuffer.val
  miso :: Wire (Bit 1) <- makeWire false

  always do
    when (phase.val =!= 0) do
      phase <== phase.val - 1

    when (phase.val === 0 .&&. requestValid.val =!= 0) do
      when (clk.val) do
        requestValid <== truncate requestValid.val # (0 :: Bit 1)
        requestBuffer <== truncate requestBuffer.val # (1 :: Bit 1)

        responseBuffer <== truncate responseBuffer.val # miso.val
        responseValid <== truncate responseValid.val # (1 :: Bit 1)

      clk <== inv clk.val
      phase <== divider.val

    when (responseValid.val === 0 .&&. requestValid.val === 0 .&&. inputs.canPeek) do
      requestBuffer <== inputs.peek
      requestValid <== ones
      inputs.consume

  return
    (SpiIO
      { fabric=
          SpiFabric
            { clk= clk.val
            , mosi= mosi
            , miso= \ x -> miso <== x
            , cs= cs.val}
      , setDivider= \ x -> divider <== x
      , setCS= \ x -> cs <== x}
    , Source
        { canPeek= responseValid.val === ones
        , peek= responseBuffer.val
        , consume= responseValid <== 0 }
    )

data Printer =
  Printer
    { canDisplay :: Bit 1
    , displayLower :: Bit 5 -> Action ()
    , displayUpper :: Bit 5 -> Action ()
    , displayHexa :: Bit 4 -> Action ()
    , displayBeginLine :: Action ()
    , displayNewLine :: Action ()}

makePrinter :: Module (Printer, Stream (Bit 8))
makePrinter = do
  wire :: Wire (Bit 8) <- makeWire dontCare
  queue :: Queue (Bit 8) <- makeQueue

  return
    ( Printer
        { canDisplay= queue.notFull
        , displayLower= \ alpha -> do
            queue.enq (zeroExtend alpha + fromAscii 'a')
        , displayUpper= \ alpha -> do
            queue.enq (zeroExtend alpha + fromAscii 'A')
        , displayBeginLine= queue.enq 10
        , displayNewLine= queue.enq 13
        , displayHexa= \ hexa -> do
            if (hexa .<. 10) then do
              queue.enq (zeroExtend hexa + fromAscii '0')
            else do
              queue.enq (zeroExtend (hexa-10) + fromAscii 'a')}
    , toStream queue)
  where
    fromAscii :: Char -> Bit 8
    fromAscii = constant . toInteger . fromEnum

makeTestSpi :: Module (Bit 1, SpiFabric)
makeTestSpi = do
  requestQ :: Queue (Bit 8) <- makeQueue
  (spi, response) <- makeSpi (toStream requestQ)

  (printer, uart) <- makePrinter
  tx <- makeTxUart (div 83_000_000 115200) uart

  counter :: Reg (Bit 32) <- makeReg 0

  let send :: Bit 8 -> Stmt () = \ byte -> do
        wait requestQ.notFull
        action do requestQ.enq byte
        wait response.canPeek
        action do response.consume

  let disable :: Stmt () = do
        send 0xFF
        action do spi.setCS 1
        send 0xFF

  let enable :: Stmt () = do
        send 0xFF
        action do spi.setCS 0
        send 0xFF

  let displayResult :: Stmt () = do
        wait printer.canDisplay
        action do printer.displayHexa (truncateLSB response.peek)
        wait printer.canDisplay
        action do printer.displayHexa (truncate response.peek)

  let displayReturn :: Stmt () = do
        wait printer.canDisplay
        action do printer.displayNewLine
        wait printer.canDisplay
        action do printer.displayBeginLine

  let sendCmd :: Bit 8 -> Bit 32 -> Bit 8 -> Stmt () = \ cmd arg crc -> do
        send 0xFF
        send (cmd .|. 0x40)
        send (slice @31 @24 arg)
        send (slice @23 @16 arg)
        send (slice @15 @8 arg)
        send (slice @7 @0 arg)
        send (crc .|. 0x01)
        while (response.peek === 0xFF) do
          send 0xFF

  let sendCmd0 :: Stmt () = do
        enable
        sendCmd 0 0 0x95
        displayResult
        displayReturn
        disable

  let sendCmd8 :: Stmt () = do
        enable
        sendCmd 8 0x01AA 0x87
        displayResult
        action do counter <== 8
        while (counter.val =!= 0) do
          action do counter <== counter.val - 1
          send 0xFF
          displayResult
        displayReturn
        disable

  let sendCmd41 :: Stmt () = do
        enable
        while (response.peek =!= 0) do
          sendCmd 55 0 0
          displayResult
          sendCmd 41 0x40000000 0
          displayResult
          displayReturn
        disable

  let sendCmd58 :: Stmt () = do
        enable
        sendCmd 58 0 0
        displayResult
        action do counter <== 8
        while (counter.val =!= 0) do
          action do counter <== counter.val - 1
          send 0xFF
          displayResult
        displayReturn
        disable

  let sendCmd16 :: Stmt () = do
        enable
        sendCmd 16 512 0
        displayResult
        displayReturn
        disable

  let readBlock :: Bit 32 -> Stmt () = \ id -> do
        enable
        sendCmd 17 id 0
        displayReturn
        action do counter <== 600
        while (counter.val =!= 0) do
          action do counter <== counter.val - 1
          send 0xFF
          displayResult
        displayReturn
        disable


  runStmt do
    action do
      spi.setDivider 128
      spi.setCS 1

      counter <== 10000

    while (counter.val =!= 0) do
      action do counter <== counter.val - 1

    enable

    action do counter <== 160

    while (counter.val =!= 0) do
      action do counter <== counter.val - 1
      send 0xFF

    sendCmd0
    sendCmd8
    sendCmd41

    action do spi.setDivider 0

    sendCmd58
    sendCmd16

    readBlock 0

  return (tx, spi.fabric)

