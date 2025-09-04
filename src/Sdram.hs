module Sdram where

import Blarney
import Blarney.Stmt
import Blarney.SourceSink
import Blarney.Arbiter
import Blarney.Option
import Blarney.Queue

import qualified Data.List as List

-- Warning: sdram_clk must be the inverse of the driver clock
data SdramFabric =
  SdramFabric
    { sdram_csn :: Bit 1                -- Those wires are used to encode
    , sdram_rasn :: Bit 1               -- the command of the current
    , sdram_casn :: Bit 1               -- operation: precharge, autorefresh,
    , sdram_wen :: Bit 1                -- read, write, modeset, nop, activate...
    , sdram_a :: Bit 13                 -- row/column address
    , sdram_ba :: Bit 2                 -- bank address
    , sdram_dqm :: Bit 2                -- byte select
    , sdram_din :: Bit 16 -> Action ()  -- input data
    , sdram_dout :: Bit 16              -- output data
    , sdram_den :: Bit 1 }              -- output data enable
  deriving(Generic, Interface)

data SdramCmd =
  SdramCmd (Bit 4)
  deriving(Generic, Bits)

-- NO Operation
nop_cmd = SdramCmd 0b1000

-- Open a bank with a row given by
-- the address bus
activate_cmd = SdramCmd 0b0101

-- Close a bank, or all the banks if
-- the bit 10 of the address bus is high
precharge_cmd = SdramCmd 0b0001

-- Refresh all the banks
autorefresh_cmd = SdramCmd 0b0100

-- Configure the SDRAM, the configuration is
-- given by the address bus
modeset_cmd = SdramCmd 0b0000

-- Read the value of a column of the currently
-- opened bank given by the address bus, the
-- bit 10 of this bus can be selected to
-- automatically close this bank after the
-- read. The value will be available tCL cycles
-- after the read (2 or 3 depending of the
-- configuration), and DQM must be clear two
-- cycles before the read value is returned.
read_cmd = SdramCmd 0b0110
write_cmd = SdramCmd 0b0010

instance FShow SdramCmd where
  fshow c =
    formatCond (c === nop_cmd) (fshow "NOP") <>
    formatCond (c === precharge_cmd) (fshow "PRECHARGE") <>
    formatCond (c === autorefresh_cmd) (fshow "AUTOREFRESH") <>
    formatCond (c === modeset_cmd) (fshow "MODESET") <>
    formatCond (c === read_cmd) (fshow "READ") <>
    formatCond (c === write_cmd) (fshow "WRITE") <>
    formatCond (c === activate_cmd) (fshow "ACTIVATE")

type NumBank = 4
type Bank = Bit (Log2 NumBank)

-- Send command to the SDRAM bus, `write` can't be called during
-- a read command due (otherwise their is a double dqm write) but
-- this must not be the case due to timing constraints
data BusController =
  BusController
    { command :: SdramCmd -> Bank -> Bit 13 -> Action ()
    , write :: Bit 16 -> Bit 2 -> Action ()
    , read :: Bit 32 }

makeBusController :: Module (BusController, SdramFabric)
makeBusController = do
  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  address :: Reg (Bit 13) <- makeReg 0
  cmd :: Reg SdramCmd <- makeDReg nop_cmd
  dqm :: Reg (Bit 2) <- makeReg 0

  bank :: Reg (Bit 2) <- makeReg 0

  dout :: Reg (Bit 16) <- makeReg 0
  den :: Reg (Bit 1) <- makeDReg false

  din_w :: Wire (Bit 16) <- makeWire dontCare
  din :: Reg (Bit 32) <- makeReg dontCare

  always do
    din <== din_w.val # upper din.val

  let controller =
        BusController
          { write= \ bytes byteEn -> do
              dout <== bytes
              dqm <== inv byteEn
              den <== true
          , read= din.val
          , command= \ c ba addr -> do
              -- DQM must be set to 0 during a read
              when (c === read_cmd) do
                dqm <== 0

              address <== addr
              bank <== ba
              cmd <== c }

  let fabric =
        SdramFabric
          { sdram_csn= at @3 (pack cmd.val)
          , sdram_wen= at @2 (pack cmd.val)
          , sdram_rasn= at @1 (pack cmd.val)
          , sdram_casn= at @0 (pack cmd.val)
          , sdram_dqm= dqm.val
          , sdram_a= address.val
          , sdram_ba= bank.val
          , sdram_dout= dout.val
          , sdram_den= den.val
          , sdram_din= \ x -> din_w <== x }

  return (controller, fabric)

type Delay = Bit 4


data SdramConfig =
  SdramConfig
    { tRP :: Integer   -- precharge time
    , tMRD :: Integer  -- time of update of the MRS register
    , tRCD :: Integer  -- activation time
    , tRC :: Integer   -- auto refresh time
    , tCL :: Integer } -- column update time

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

makeSdram ::
  SdramConfig
  -> Source (Bit 23, Bit 32, Bit 4)
  -> Module (SdramFabric, Source (Bit 32))
makeSdram config inputs = do
  outputQ :: Queue (Bit 32) <- makeQueue

  let tRP :: Bit 4 = lit config.tRP
  let tMRD :: Bit 4 = lit config.tMRD
  let tRCD :: Bit 4 = lit config.tRCD
  let tRC :: Bit 4 = lit config.tRC
  let tCL :: Bit 4 = lit config.tCL

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

  cmdAddr :: Reg (Bit 23) <- makeReg 0
  cmdData :: Reg (Bit 32) <- makeReg 0
  cmdMask :: Reg (Bit 4) <- makeReg 0
  let isRead = cmdMask.val === 0

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  init :: Reg (Bit 1) <- makeReg false
  banks :: [Reg (Option (Bit 13))] <- replicateM 4 (makeReg none)

  (controller, fabric) <- makeBusController

  always do
    when (inv init.val .&&. state.val === idle_st) do
      if (counter.val + 1 .>=. initCycles) then do
        state <== refresh1_st
        counter <== 0
      else do
        counter <== counter.val + 1

    when (state.val =!= idle_st .||. init.val) do
      if (state.val =!= refresh2_st) then do
        counter <== counter.val + 1
      else do
        counter <== 0

      when (state.val === wait_st) do
        if (delay.val === 0) then do
          state <== nextState.val
        else do
          delay <== delay.val - 1

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
        let bank :: Bit 2 = slice @9 @8 cmdAddr.val
        let row :: Bit 13 = slice @22 @10 cmdAddr.val
        let col :: Bit 9 = slice @7 @0 cmdAddr.val # 0

        if (banks!bank).val.valid then do
          if (banks!bank).val.val === row then do

            if isRead then do
              until readready_st tCL
              controller.command read_cmd bank (0 # col)
            else do
              controller.write (lower cmdData.val) (lower cmdMask.val)
              controller.command write_cmd bank (0 # col)
              state <== write1_st
          else do
            -- address[10] is 0 so we only close this bank
            banks!bank <== none
            until rdwr_st (tRP-2)
            controller.command precharge_cmd bank 0
        else do
          until rdwr_st (tRCD-2)
          banks!bank <== some row
          controller.command activate_cmd bank row

      when (state.val === write1_st) do
        controller.write (upper cmdData.val) (upper cmdMask.val)
        until idle_st (tCL-1)

      when (state.val === readready_st) do
        dynamicAssert outputQ.notFull "enq into an empty queue"
        outputQ.enq controller.read
        state <== idle_st

      when (state.val === refresh1_st) do
        controller.command precharge_cmd 0 (2^10)
        until (init.val ? (refresh2_st, config_st)) (tRP-2)
        sequence_ [b <== none | b <- banks]

      when (state.val === config_st) do
        controller.command modeset_cmd 0 addrMode
        until refresh2_st (tMRD-2)

      when (state.val === refresh2_st) do
        controller.command autorefresh_cmd 0 0
        until (init.val ? (idle_st, refresh2_st)) (tRC-2)
        init <== true

  return (fabric, toSource outputQ)

-- This module control the bus timings constraints using the
-- read/write fields:
--    The data bus is used in burts by each bank:
--        - Reads use the bus for N cycles, but accesses are delayed
--          by 2 or 3 cycles depending of the configuration of the chip
--          and the `dqm` field is read 2 cycles before the value is
--          returned
--        - Writes use the bus for N cycles
--    The strategy is the following:
--        - During a read, all reads are blocked during N cycles only,
--          so reads can be pipelined, but writes must wait for the
--          full burst to finish before starting to not corrupt DQM,
--          this can take up to N+3 cycles depending of the choosen
--          read latency
--        - During a write, all read/write opreations are blocked
--          for N cycles because they require the DQM bus
--
--    As example with 3 cycles of read latency, and bursts of length 2,
--    if we read (d0, d1) and (d2, d3) with two requests then write (d4,d5):
--
--      | time    | 0    | 1    | 2   | 3   | 4   | 5   | 6   | 7     | 8   |
--      | command | read | read | nop | nop | nop | nop | nop | write | nop |
--      | dqm     | X    | 0    | 0   | X   | X   | X   | X   | be1   | be2 |
--      | data    | X    | X    | X   | d0  | d1  | d2  | d3  | d4    | d5  |
--
--    Here all the read/write commands are on different banks, otherwise
--    additional timing constraints may apply. And `X` means that we don't
--    care about the value of the bus.
--
--    The write burst can't be done earlier because the DQM and data bus
--    are busy before with of the two read commands.
--
--    This strategy is a greedy algorithm because I don't try to reorder
--    reads/writes to have better performances (one can imagine doing a
--    read in priority to a write to pipeline it with another read, or
--    reordering two reads to reuse an open bank) but in practice if
--    offer good enough performances for my use cases.
--
--    Also in case of bursts of length one, it's possible to do better, as
--    example (still with a read latency of 3), we can do something like
--    this:
--
--      | time    | 0    | 1    | 2     | 3   |
--      | command | read | nop  | write | nop |
--      | dqm     | X    | 0    | be1   | X   |
--      | data    | X    | X    | d0    | d1  |
--
--    Here we perform a write of d0 between the read of the DQM bus,
--    and the output of the read value d1.
--
--    And with busts of bigger size it's sometime possible to start a read
--    sequence before the end of a write burst
--
--      | time    | 0     | 1     | 2   | 3   | 4   | 5   |
--      | command | write | read  | nop | nop | nop | nop |
--      | dqm     | be1   | be2   | 0   | X   | X   | X   |
--      | data    | d0    | d1    | X   | X   | d3  | d4  |
--
--    But I decided to ignore those cases here, instead I wait t=2 to start
--    the read request.
--
-- It also defines the global/locals primitives to control the
-- SDRAM delay relative to each commands:
--    - locals will allow each banks to start a timer only if the previous
--      timer of the bank already finish
--    - global start the timer of each banks only if all the previous
--      timers already finished
data DelayController =
  DelayController
    { global :: Sink Delay
    , locals :: [Sink Delay]
    , read :: Sink (Delay, Delay)
    , write :: Sink Delay }

makeDelayController :: Int -> Module DelayController
makeDelayController numBank = do
  delays :: [Reg Delay] <- replicateM numBank (makeReg 0)

  rdDelay :: Reg Delay <- makeReg 0
  wrDelay :: Reg Delay <- makeReg 0

  always do
    when (rdDelay.val =!= 0) do rdDelay <== rdDelay.val - 1
    when (wrDelay.val =!= 0) do wrDelay <== wrDelay.val - 1
    sequence_ [when (d.val =!= 0) do d <== d.val - 1 | d <- delays]

  let locals =
        [ Sink
            { canPut= d.val === 0
            , put= d.writeReg }
        | d <- delays ]

  let global =
        Sink
          { canPut= andList [d.val === 0 | d <- delays]
          , put= \ x -> sequence_ [d <== x | d <- delays] }

  let read =
        Sink
          { canPut= rdDelay.val === 0
          , put= \ (x, y) -> do
              rdDelay <== x
              when (y .>. wrDelay.val) do
                wrDelay <== y }

  let write =
        Sink
          { canPut= wrDelay.val === 0
          , put= \ x -> do
              wrDelay <== x
              when (x .>. rdDelay.val) do
                rdDelay <== x }

  return DelayController{locals, global, read, write}


data BankController =
  BankController
    { reqs :: Sink (Bit 13, Bit 9, Bit 32, Bit 4)
    , resps :: Source (Bit 32)
    , close :: Action ()
    , isIdle :: Bit 1 }

makeBankController ::
  SdramConfig
  -> Bank
  -> ArbiterClient
  -> BusController
  -> DelayController
  -> Module BankController
makeBankController config bank arbiter bus delays = do
  let local = delays.locals!bank

  state :: Reg (Bit 3) <- makeReg 0

  open :: Reg (Option (Bit 13)) <- makeReg none

  let close = open <== none
  let isIdle = state.val === 0

  outputQ :: Queue (Bit 32) <- makeQueue

  lane :: Reg (Bit 32) <- makeReg dontCare
  mask :: Reg (Bit 4) <- makeReg dontCare
  row :: Reg (Bit 13) <- makeReg dontCare
  col :: Reg (Bit 9) <- makeReg dontCare

  let tRP :: Bit 4 = lit config.tRP
  let tMRD :: Bit 4 = lit config.tMRD
  let tRCD :: Bit 4 = lit config.tRCD
  let tRC :: Bit 4 = lit config.tRC
  let tCL :: Bit 4 = lit config.tCL

  doRead0 :: Wire (Bit 1) <- makeWire false
  doRead1 :: Wire (Bit 1) <- makeWire false
  doWrite0 :: Wire (Bit 1) <- makeWire false
  doActivate :: Wire (Bit 1) <- makeWire false
  doPrecharge :: Wire (Bit 1) <- makeWire false

  always do
    when (state.val =!= 0 .&&. local.canPut) do
      if open.val.valid then do
        if open.val.val === row.val then do
          when (state.val === 1 .&&. delays.read.canPut .&&. outputQ.notFull) do
            doRead0 <== true
            arbiter.request

          when (state.val === 2) do
            doRead1 <== true

          when (state.val === 3 .&&. delays.write.canPut) do
            doWrite0 <== true
            arbiter.request

        else do
          doPrecharge <== true
          arbiter.request
      else do
        doActivate <== true
        arbiter.request

    when arbiter.grant do
      when doActivate.val do
        bus.command activate_cmd bank row.val
        open <== some row.val
        local.put (tRCD-1)

      when doPrecharge.val do
        bus.command precharge_cmd bank 0
        local.put (tRP-1)
        open <== none

      when doRead0.val do
        bus.command read_cmd bank (0 # col.val)
        delays.read.put (2-1, tCL+2-1)
        local.put (tCL+2-1)
        state <== 2

      when doWrite0.val do
        bus.write (lower lane.val) (lower mask.val)
        bus.command write_cmd bank (0 # col.val)
        delays.write.put (2-1)
        local.put (tCL-2)
        state <== 4

    when doRead1.val do
      outputQ.enq bus.read
      state <== 0

    when (state.val === 4) do
      bus.write (upper lane.val) (upper mask.val)
      state <== 0

  return
    BankController
      { reqs=
          Sink
            { canPut= state.val === 0
            , put= \ (r, c, d, m) -> do
                state <== m === 0 ? (1,3)
                lane <== d
                mask <== m
                row <== r
                col <== c}
      , close
      , isIdle
      , resps= toSource outputQ }

makeSdram2 ::
  SdramConfig
  -> Source (Bit 23, Bit 32, Bit 4)
  -> Module (SdramFabric, Source (Bit 32))
makeSdram2 config inputs = do
  bankQ :: Queue Bank <- makeSizedQueueCore 4

  let tRP :: Bit 4 = lit config.tRP
  let tMRD :: Bit 4 = lit config.tMRD
  let tRCD :: Bit 4 = lit config.tRCD
  let tRC :: Bit 4 = lit config.tRC
  let tCL :: Bit 4 = lit config.tCL

  let clkMhz :: Bit 16  = 25
  let initCycles :: Bit 16 = 100 * clkMhz

  let rfCycles :: Bit 16 = 7 * clkMhz

  let addrMode :: Bit 13 = 0b000_0_00_011_0_001

  counter :: Reg (Bit 16) <- makeReg 0

  state :: Reg (Bit 3) <- makeReg idle_st

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  init :: Reg (Bit 1) <- makeReg false

  (controller, fabric) <- makeBusController
  delays <- makeDelayController (valueOf @NumBank)

  arbiter <- makeFairArbiter (valueOf @NumBank)

  banks :: [BankController] <-
    sequence
      [ makeBankController config (lit bank) client controller delays
      | (client, bank) <- zip arbiter [0..] ]

  always do
    when (inv init.val .&&. state.val === idle_st) do
      if (counter.val + 1 .>=. initCycles) then do
        state <== refresh1_st
        counter <== 0
      else do
        counter <== counter.val + 1

    when (state.val =!= idle_st .||. init.val) do
      if (state.val =!= refresh2_st) then do
        counter <== counter.val + 1
      else do
        counter <== 0

      when (state.val === idle_st) do
        if counter.val .>=. rfCycles then do
          when (andList [b.isIdle | b <- banks]) do
            state <== refresh1_st

        else do
          when inputs.canPeek do
            let (addr, lane, mask) = inputs.peek
            let col = slice @9 @2 addr # 0
            let row = slice @22 @10 addr
            let ba = slice @1 @0 addr

            forM_ (zip banks [0..]) \ (b,i) -> do
              when (ba === lit i .&&. b.reqs.canPut .&&. bankQ.notFull) do
                when (mask === 0) do bankQ.enq (lit i)
                b.reqs.put (row,col,lane,mask)
                inputs.consume

      when delays.global.canPut do
        when (state.val === refresh1_st) do
          controller.command precharge_cmd 0 (2^10)
          state <== init.val ? (refresh2_st, config_st)
          sequence_ [b.close | b <- banks]
          delays.global.put (tRP-2)

        when (state.val === config_st) do
          controller.command modeset_cmd 0 addrMode
          delays.global.put (tMRD-1)
          state <== refresh2_st

        when (state.val === refresh2_st) do
          controller.command autorefresh_cmd 0 0
          state <== init.val ? (idle_st, refresh2_st)
          delays.global.put (tRC-2)
          init <== true

  let output =
        Source
          { canPeek=
              bankQ.canDeq .&&.
                orList
                  [ b.resps.canPeek .&&. lit i === bankQ.first
                  | (b,i) <- zip banks [0..]]
          , peek=
              select
                [ lit i === bankQ.first --> b.resps.peek
                | (b,i) <- zip banks [0..]]
          , consume= do
              bankQ.deq
              sequence_
                [ when (lit i === bankQ.first) do
                    b.resps.consume
                | (b,i) <- zip banks [0..]]}

  return (fabric, output)


makeTestSdram :: Module SdramFabric
makeTestSdram = do

  inputs :: Queue (Bit 23, Bit 32, Bit 4) <- makeQueue

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  let hash :: Bit 23 -> Bit 32 = \ x ->
        let y :: Bit 32 = zeroExtend x in
        (y .<<. (20 :: Bit 5)) .^. y

  let config =
        SdramConfig
          { tRP = 3
          , tMRD = 2
          , tRCD = 3
          , tRC = 9
          , tCL = 3 }

  (fabric, outputs) <- makeSdram config (toSource inputs)

  address :: Reg (Bit 23) <- makeReg 0
  addressQ :: Queue (Bit 23) <- makeSizedQueueCore 4

  let numTests = 10000

  runStmt do
    while (address.val .<. numTests) do
      wait inputs.notFull
      action do
        address <== address.val + 1
        inputs.enq (address.val, hash address.val, 0b1111)

    action (address <== 0)

    while true do
      wait (inputs.notFull .&&. addressQ.notFull)
      action do
        inputs.enq (address.val, 0, 0b0000)
        address <== address.val + 1 === numTests ? (0, address.val + 1)
        addressQ.enq address.val

  always do
    when (outputs.canPeek .&&. addressQ.canDeq) do
      display
        cycle.val " counter: " addressQ.first
        " value: 0x" (formatHex 8 outputs.peek)
      outputs.consume
      addressQ.deq

      dynamicAssert
        (hash addressQ.first === outputs.peek)
        "unexpected value"

      --when (addressQ.first + 1 === numTests) do
      --  finish

  return fabric

