import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.TaggedUnion
import System.Environment
import Blarney.Arbiter
import Blarney.Ehr

import Utils


import TileLink
import TileLink.GetPut

import CPU
import qualified Spi
import qualified Instr

import qualified Uart
import qualified Cache
import qualified Core

type Byte i = Bit (8*i)

testRegFile :: Module ()
testRegFile = do
  rf :: RegFile (Bit 10) (Bit 32) <- makeRegFile

  r :: Reg (Bit 32) <- makeRegU

  always do
    rf.update 0 r.val
    rf.update 1 r.val
    r <== rf.index 32
    r <== rf.index 33

  return ()

testIf :: Module ()
testIf = do
  always do
    x <- if false then return false else return true
    display x
    finish

testRAM :: Module ()
testRAM = do
  ram :: RAM (Bit 10) (Bit 32) <- makeDualRAM

  counter :: Reg (Bit 32) <- makeReg 0

  always do
    when (counter.val === 1) do
      display "load at counter: " counter.val
      ram.load 0

    when (counter.val === 2) do
      display "store at counter: " counter.val
      ram.store 0 42

    if (counter.val .<=. 10) then do
      display "counter: " counter.val " out: " ram.out
    else do
      finish

    counter <== counter.val + 1

top :: Module ()
top = do
  -- Create a register
  counter :: Ehr (Bit 32) <- makeEhr 2 0

  fifo :: Queue (Bit 32) <- makePipelineQueue 1

  ram :: RAM (Bit 32) (Bit 32) <- makeDualRAMForwardInit "Mem.hex"

  arbiter :: [ArbiterClient] <- makeFairArbiter 16

  rf :: RegFile (Bit 10) (Bit 10) <- makeRegFile

  let instr = Instr.decodeInstr ram.out
  let instr2 = Instr.decodeInstr ram.out
  let pc = (counter.read 0 .<<. (2 :: Bit 5)) + 0x80000000

  always do
    when fifo.canDeq do
      display "pc: " (formatHex 0 pc) " " (fshow instr)
      counter.write 0 (counter.read 0 + 1)
      fifo.deq

    when fifo.notFull do
      ram.load (counter.read 1)
      fifo.enq (counter.read 1)

    when (counter.read 0 === 100) do
      finish

    sequence_
      [when (untypedAt i (counter.read 0)) do
        a.request
      | (a,i) <- zip arbiter [0..]]

    sequence_
      [when (a.grant) do
        display "Grant index: " i " at cycle: " (formatHex 10 (counter.read 0))
      | (a,i :: Int) <- zip arbiter [0..]]

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> do
        --simulate top
        --simulate CPU.makeCPU
        --simulate Uart.testUart2
        --simulate testRAM
        --simulate Cache.testCache
        simulate testIf
     | otherwise -> do
        writeVerilogTop Cache.testCache "TestCache" "Verilog/"
        writeVerilogTop top "Main" "Verilog/"
        writeVerilogTop CPU.makeCPU "CPU" "Verilog/"
        writeVerilogModule Uart.testUart "Uart" "Verilog/"
        writeVerilogTop testRegFile "Rf" "Verilog/"
        writeVerilogTop testRAM "TestRam" "Verilog/"
        writeVerilogModule Core.makeFakeTestCore "TestCore" "Verilog/"
        --writeVerilogModule makeTestGetPut "TestCore" "Verilog/"
        writeVerilogModule Spi.makeTestSpi "TestSpi" "Verilog/"
