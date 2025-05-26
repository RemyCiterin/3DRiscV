module Uart where

import Blarney
import Blarney.Option
import Blarney.SourceSink
import Blarney.Stream
import Blarney.Queue

makeRxUart :: Integer -> Bit 1 -> Module (Stream (Bit 8))
makeRxUart timePerBit rx = do
  -- number of positive measures received
  nbPositive :: Reg (Bit 32) <- makeReg 0

  count :: Reg (Bit 32) <- makeReg 0

  busy :: Reg (Bit 1) <- makeReg false

  buf :: Reg (Bit 10) <- makeReg 0
  valid :: Reg (Bit 10) <- makeReg 0

  validCopy :: Reg (Bit 1) <- makeReg false
  bufCopy :: Reg (Bit 8) <- makeReg 0

  zeroCount :: Reg (Bit 32) <- makeReg 0

  always do

    if (busy.val) then do
      if (valid.val === ones) then do
        --when (at @0 buf.val === 0 .&&. at @9 buf.val === 1) do
        bufCopy <== slice @8 @1 buf.val
        validCopy <== true

        valid <== 0
        zeroCount <== 0
        busy <== false
      else do

        if (count.val === 0) then do
          count <== fromInteger timePerBit

          let newBit = 2 .*. nbPositive.val .>. fromInteger timePerBit

          nbPositive <== 0

          if (valid.val === 0 .&&. newBit) then do
            zeroCount <== 0
            busy <== false
          else do
            valid <== (0b1 :: Bit 1) # upper valid.val
            buf <== newBit # upper buf.val

        else do
          when (rx === 1) do
            nbPositive <== nbPositive.val + 1

          count <== count.val - 1
    else do
      if (zeroCount.val .>=. 10) then do
        count <== fromInteger timePerBit - 10
        nbPositive <== 0
        busy <== true
        valid <== 0
      else do
        zeroCount <== rx === 0 ? (zeroCount.val+1, 0)


  return Source {
    consume = do validCopy <== false,
    canPeek = validCopy.val,
    peek = bufCopy.val
  }

makeTxUart :: Integer -> Stream (Bit 8) -> Module (Bit 1)
makeTxUart timePerBit stream = do
  tx :: Wire (Bit 1) <- makeWire 0

  valid :: Reg (Bit 24) <- makeReg ones
  buf   :: Reg (Bit 24) <- makeReg ones
  count :: Reg (Bit 32) <- makeReg 0

  always do
    when (valid.val === 0) do
      tx <== 1

      when (stream.canPeek) do
        buf <== ones # stream.peek # (0b0 :: Bit 1)
        display stream.peek
        stream.consume
        valid <== ones
        count <== 0
    when (valid.val =!= 0) do
      tx <== at @0 buf.val

      if (count.val .>=. fromInteger timePerBit) then do
        valid <== (0b0 :: Bit 1) # upper valid.val
        buf   <== (0b1 :: Bit 1) # upper buf.val
        display "valid: " (formatBin 24 valid.val)
        display "buffer: " (formatBin 24 buf.val)
        count <== 0
      else do
        count <== count.val + 1

  return tx.val

data TestUartIfc = TestUartIfc {
    tx :: Bit 1
  , led :: Bit 8
  } deriving(Generic,Interface)

testUart :: Bit 1 -> Module TestUartIfc
testUart rx = mdo
  rxuart <- makeRxUart 217 rx
  tx <- makeTxUart 217 rxuart

  led :: Reg (Bit 8) <- makeReg 0

  always do
    when (rxuart.canPeek) do
      led <== rxuart.peek

  return TestUartIfc {tx, led = led.val}

testUart2 :: Module ()
testUart2 = do
  uart <- testUart 1
  return ()
