module Sdram where

import Blarney
import Blarney.Stmt
import Blarney.SourceSink
import Blarney.Option
import Blarney.Queue

-- Warning: sdram_clk must be the inverse of the driver clock
data SdramFabric =
  SdramFabric
    { sdram_csn :: Bit 1                -- Those wires are used to encode
    , sdram_rasn :: Bit 1               -- the command of the current
    , sdram_casn :: Bit 1               -- operation: precharge, autorefresh,
    , sdram_wen :: Bit 1                -- read, write, modeset, nop, activate...
    , sdram_a :: Bit 13                 -- row/column address
    , sdram_ba :: Bit 2                 -- back address
    , sdram_dqm :: Bit 2                -- byte select
    , sdram_din :: Bit 16 -> Action ()  -- input data
    , sdram_dout :: Bit 16              -- output data
    , sdram_den :: Bit 1 }              -- output data enable
  deriving(Generic, Interface)

data SdramCmd =
  SdramCmd (Bit 4)
  deriving(Generic, Bits)

nop_cmd = SdramCmd 0b1000
precharge_cmd = SdramCmd 0b0001
autorefresh_cmd = SdramCmd 0b0100
modeset_cmd = SdramCmd 0b0000
read_cmd = SdramCmd 0b0110
write_cmd = SdramCmd 0b0010
activate_cmd = SdramCmd 0b0101

idle_st, refresh1_st, refresh2_st, config_st :: Bit 3
rdwr_st, readready_st, wait_st, write1_st :: Bit 3
idle_st = 0
refresh1_st = 1
refresh2_st = 2
config_st = 3
rdwr_st = 4
readready_st = 5
wait_st = 6
write1_st = 7

data SdramConfig =
  SdramConfig
    { tRP :: Integer   -- precharge time
    , tMRD :: Integer  -- time of update of the MRS register
    , tRCD :: Integer  -- activation time
    , tRC :: Integer   -- auto refresh time
    , tCL :: Integer } -- column update time

makeSdram ::
  SdramConfig
  -> Source (Bit 24, Bit 32, Bit 4)
  -> Module (SdramFabric, Source (Bit 32))
makeSdram config inputs = do
  outputQ :: Queue (Bit 32) <- makeQueue

  let tRP :: Bit 4 = constant config.tRP
  let tMRD :: Bit 4 = constant config.tMRD
  let tRCD :: Bit 4 = constant config.tRCD
  let tRC :: Bit 4 = constant config.tRC
  let tCL :: Bit 4 = constant config.tCL

  let clkMhz :: Bit 16  = 25
  let initCycles :: Bit 16 = 100 * clkMhz

  let rfCycles :: Bit 16 = 7 * clkMhz

  let addrMode :: Bit 13 = 0b000_0_00_011_0_001

  counter :: Reg (Bit 16) <- makeReg 0
  delay :: Reg (Bit 4) <- makeReg 0

  state :: Reg (Bit 3) <- makeReg idle_st
  nextState :: Reg (Bit 3) <- makeReg dontCare

  let until :: Bit 3 -> Bit 4 -> Action () = \ st t -> do
        state <== wait_st
        nextState <== st
        delay <== t

  cmd :: Reg SdramCmd <- makeReg nop_cmd
  address :: Reg (Bit 13) <- makeReg 0
  dqm :: Reg (Bit 2) <- makeReg 0b11

  cmdAddr :: Reg (Bit 24) <- makeReg 0
  cmdData :: Reg (Bit 32) <- makeReg 0
  cmdMask :: Reg (Bit 4) <- makeReg 0
  let isRead = cmdMask.val === 0

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  init :: Reg (Bit 1) <- makeReg false
  banks :: [Reg (Option (Bit 13))] <- replicateM 2 (makeReg none)

  dout :: Reg (Bit 16) <- makeReg 0
  den :: Reg (Bit 1) <- makeDReg false

  din_w :: Wire (Bit 16) <- makeWire dontCare
  din :: Reg (Bit 32) <- makeReg dontCare

  always do
    din <== din_w.val # upper din.val

  always do
    when (inv init.val .&&. state.val === idle_st) do
      if (counter.val + 1 .>=. initCycles) then do
        state <== refresh1_st
        counter <== 0
      else do
        counter <== counter.val + 1

    when (state.val =!= idle_st .||. init.val) do

      when (state.val === wait_st) do
        if (delay.val === 0) then do
          state <== nextState.val
        else do
          delay <== delay.val - 1
        cmd <== nop_cmd

      when (state.val === idle_st) do
        if (counter.val .>=. rfCycles) then do
          state <== refresh1_st
        else when (inputs.canPeek .&&. outputQ.notFull) do
          let (addr, lane, mask) = inputs.peek

          state <== rdwr_st
          cmdMask <== mask
          cmdData <== lane
          cmdAddr <== addr
          inputs.consume

      when (state.val === rdwr_st) do
        let bank = slice @10 @9 cmdAddr.val
        let row = slice @23 @11 cmdAddr.val
        let col = slice @8 @0 cmdAddr.val

        if (banks!bank).val.valid then do
          if (banks!bank).val.val === row then do
            cmd <== isRead ? (read_cmd, write_cmd)
            address <== 0 # col

            if isRead then do
              until readready_st tCL
              dqm <== 0
            else do
              dout <== slice @15 @0 cmdData.val
              dqm <== inv (slice @1 @0 cmdMask.val)
              state <== write1_st
              den <== true
          else do
            -- address[10] is 0 so we only close this bank
            cmd <== precharge_cmd
            until rdwr_st (tRP-2)
            banks!bank <== none
            address <== 0
        else do
          banks!bank <== some row
          until rdwr_st (tRCD-2)
          cmd <== activate_cmd
          address <== row

      when (state.val === write1_st) do
        dout <== slice @31 @16 cmdData.val
        dqm <== inv (slice @3 @2 cmdMask.val)
        until idle_st (tCL-1)
        cmd <== nop_cmd
        address <== 0
        den <== true

      when (state.val === readready_st) do
        dynamicAssert outputQ.notFull "enq into an empty queue"
        outputQ.enq din.val
        state <== idle_st

      when (state.val === refresh1_st) do
        cmd <== precharge_cmd
        until (init.val ? (refresh2_st, config_st)) (tRP-2)
        address <== (2^10)
        sequence_ [b <== none | b <- banks]

      when (state.val === config_st) do
        cmd <== modeset_cmd
        address <== addrMode
        until refresh2_st (tMRD-2)

      when (state.val === refresh2_st) do
        cmd <== autorefresh_cmd
        until (init.val ? (idle_st, refresh2_st)) (tRC-2)
        init <== true
        dqm <== 0

  let fabric =
        SdramFabric
          { sdram_csn= at @3 (pack cmd.val)
          , sdram_wen= at @2 (pack cmd.val)
          , sdram_rasn= at @1 (pack cmd.val)
          , sdram_casn= at @0 (pack cmd.val)
          , sdram_dqm= dqm.val
          , sdram_a= address.val
          , sdram_ba= state.val === config_st ? (0, slice @10 @9 address.val)
          , sdram_dout= dout.val
          , sdram_den= den.val
          , sdram_din= \ x -> din_w <== x }

  return (fabric, toSource outputQ)


makeTestSdram :: Module SdramFabric
makeTestSdram = do

  inputs :: Queue (Bit 24, Bit 32, Bit 4) <- makeQueue

  let config =
        SdramConfig
          { tRP = 3
          , tMRD = 2
          , tRCD = 3
          , tRC = 9
          , tCL = 3 }

  (fabric, outputs) <- makeSdram config (toSource inputs)

  runStmt do
    wait inputs.notFull
    action do
      inputs.enq (0, 42, 0b1111)
    wait inputs.notFull
    action do
      inputs.enq (0, 42, 0b0000)
    wait outputs.canPeek
    action do
      display outputs.peek
      outputs.consume
    action finish

  return fabric
