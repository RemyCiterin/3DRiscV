import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.TaggedUnion
import System.Environment
import Ehr

import Utils
import Decode

import Arbiter

import TileLink

import CPU
import qualified Instr

import qualified Uart
import qualified Cache

type Byte i = Bit (8*i)

top :: Module ()
top = do
  -- Create a register
  counter :: Ehr (Bit 32) <- makeEhr 2 0

  fifo :: Queue (Bit 32) <- makePipelineQueue 1

  ram :: RAM (Bit 32) (Bit 32) <- makeDualRAMForwardInit "Mem.hex"

  arbiter :: [ArbiterClient] <- makeFairArbiter 16

  rf :: RegFile (Bit 10) (Bit 10) <- makeRegFile

  let instr = decodeInstr ram.out
  let instr2 = Instr.decodeInstr ram.out
  let pc = (counter.read 0 .<<. (2 :: Bit 5)) + 0x80000000

  always do
    when fifo.canDeq do
      if instr.valid then
        display "pc: " (formatHex 0 pc) " " (fshowInstr instr.val)
      else
        display "pc: " (formatHex 10 $ counter.read 0) " Invalid"
      display instr2
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
        simulate Cache.testCache
     | otherwise -> do
        writeVerilogTop Cache.testCache "TestCache" "Verilog/"
        writeVerilogTop top "Main" "Verilog/"
        writeVerilogTop CPU.makeCPU "CPU" "Verilog/"
        writeVerilogModule Uart.testUart "Uart" "Verilog/"
