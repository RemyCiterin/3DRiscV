import Blarney
import Blarney.Queue
import Blarney.Option
import System.Environment
import Blarney.Arbiter
import Blarney.Utils
import Blarney.Ehr
import Blarney.ADT



import TileLink
import TileLink.GetPut
import MulDiv

import qualified Spi
import qualified Instr
import qualified Sdram

import qualified Gpu
import qualified Uart
import qualified Cache
import qualified Core
import qualified DDR3
import qualified Oled
import TileLink.CoherentBCache

import Soc

main :: IO ()
main = do
  args <- getArgs
  if | "--simulate" `elem` args -> do
        pure ()
     | "--formal" `elem` args -> do
        verifyMultiplier
     | otherwise -> do
        writeVerilogTop Cache.testCache "TestCache" "Verilog/"
        writeVerilogModule Uart.testUart "Uart" "Verilog/"
        --writeVerilogModule Core.makeTestCore "TestCore" "Verilog/"
        writeVerilogModule Soc.makeUlx3s "SocUlx3s" "Verilog/"
        writeVerilogModule Soc.makeTestCore "TestCore" "Verilog/"
        --writeVerilogModule Soc.makeUlx3s "TestCore" "Verilog/"
        writeVerilogModule Spi.makeTestSpi "TestSpi" "Verilog/"
        writeVerilogModule DDR3.makeWrapperDDR3 "TestDDR3" "Verilog/"
        writeVerilogModule Sdram.makeTestSdram "TestSdram" "Verilog/"
        writeVerilogModule (Oled.makeOled 83) "TestOled" "Verilog/"
