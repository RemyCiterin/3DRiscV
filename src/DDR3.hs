module DDR3 where

import Blarney
import Blarney.Queue
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Utils
import Blarney.Stmt

import Uart
import TileLink

data DDR3Outputs =
  DDR3Outputs
    { wb_addr :: Bit 32
    , wb_we :: Bit 1
    , wb_cyc :: Bit 1
    , wb_stb :: Bit 1
    , wb_data :: Bit 64
    , wb_sel :: Bit 8 }
  deriving(Generic, Interface, Bits)

data DDR3Inputs =
  DDR3Inputs
    { wb_stall :: Bit 1
    , wb_ack :: Bit 1
    , wb_err :: Bit 1
    , wb_data :: Bit 64 }
  deriving(Generic, Bits, Interface)


data DDR3Debug =
  DDR3Debug
    { led :: Bit 8
    , tx :: Bit 1 }
  deriving(Generic, Bits, Interface)

makeWrapperDDR3 :: DDR3Inputs -> Module (DDR3Outputs, DDR3Debug)
makeWrapperDDR3 inputs = do
  requests :: Queue (Bit 1, Bit 32, Bit 64, Bit 8) <- makeQueue
  responses :: Queue (Bit 64) <- makeQueue
  busy :: Reg (Bit 1) <- makeReg false

  let doRequest =
        requests.canDeq
        .&&. inv busy.val
        .&&. inv inputs.wb_stall
        .&&. responses.notFull

  always do
    when doRequest do
      busy <== true
      requests.deq

    when inputs.wb_ack do
      responses.enq inputs.wb_data
      busy <== false

  let outputs =
        let (wb_we, wb_addr, wb_data, wb_sel) = requests.first in
        DDR3Outputs
          { wb_stb= doRequest
          , wb_cyc= true
          , wb_addr
          , wb_data
          , wb_sel
          , wb_we }

  printQ :: Queue (Bit 8) <- makeQueue
  tx <- makeTxUart (div 83_000_000 115200) (toSource printQ)

  let debug =
        DDR3Debug
          { led= printQ.first
          , tx }

  timer :: Reg (Bit 32) <- makeReg 0
  always do
    when (timer.val .>. 0) do
      timer <== timer.val - 1

  runStmt do
    action do timer <== 100_000_000
    wait (timer.val === 0)

    wait printQ.notFull
    action do
      printQ.enq 0x42

    wait requests.notFull
    action do
      requests.enq (true, 0, 0x44, 0b1111)
    wait responses.canDeq
    action do
      printQ.enq 0x43
      responses.deq

    wait requests.notFull
    action do
      requests.enq (false, 0, dontCare, 0b1111)

    wait (responses.canDeq .&&. printQ.notFull)
    action do
      printQ.enq (lower responses.first)
      responses.deq

  return (outputs, debug)
