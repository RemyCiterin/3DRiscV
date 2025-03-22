import Blarney
import Blarney.Option
import Blarney.TaggedUnion
import System.Environment
import Ehr

import Utils
import Decode

type Byte i = Bit (8*i)

top :: Module ()
top = do
  -- Create a register
  counter :: Ehr (Bit 32) <- makeEhr 2 0

  fifo :: Fifo (Bit 32) <- makePipelineFifo 2

  ram :: RAM (Bit 32) (Bit 32) <- makeDualRAMForwardInit "Mem.hex"

  let instr = decodeInstr ram.out
  let pc = (counter.read 0 .<<. (2 :: Bit 5)) + 0x80000000

  always do
    when fifo.canDeq do
      if instr.valid then
        display "pc: " (formatHex 0 pc) " " (fshowInstr instr.val)
      else
        display "pc: " (formatHex 10 $ counter.read 0) " Invalid"
      counter.write 0 (counter.read 0 + 1)
      fifo.deq

    when fifo.canEnq do
      ram.load (counter.read 1)
      fifo.enq 0

    when (counter.read 0 === 100) do
      finish


main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> simulate top
     | otherwise -> writeVerilogTop top "Main" "Verilog/"
