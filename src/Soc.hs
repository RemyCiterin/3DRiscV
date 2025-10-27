module Soc where

import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Connectable
import Blarney.Utils
import Blarney.ADT

import Clint
import System
import Screen
import Uart
import Core
import Spi
import Gpu

import TileLink
import TileLink.CoherentBCache
import TileLink.Broadcast

import Sdram

makeTLSdram :: forall p.
  ( KnownTLParams p
  , 4 ~ LaneWidth p
  , 32 ~ AddrWidth p )
    => Bit (SinkWidth p)
    -> Bit (AddrWidth p)
    -> Module (SdramFabric, TLSlave p)
makeTLSdram sink lowerBound = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneLogSize :: TLSize = constant $ toInteger $ log2 $ valueOf @(LaneWidth p)

  let config =
        SdramConfig
          { tRP = 3
          , tMRD = 2
          , tRCD = 3
          , tRC = 9
          , tCL = 3 }

  inputs :: Queue (Bit 23, Bit 32, Bit 4) <- makeQueue
  (fabric, outputs) <- withName "sdram" $ makeSdram2 config (toSource inputs)

  -- Queue between the stages 1 and 2
  queue :: Queue (ChannelD p) <- makeSizedQueueCore 4
  queueA :: Queue (ChannelA p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue

  let channelD = toSink queueD
  let channelA = toSource queueA
  size :: Reg TLSize <- makeReg 0
  index :: Reg (Bit 23) <- makeReg dontCare

  always do
    when (channelA.canPeek .&&. queue.notFull .&&. inputs.notFull) do
      dynamicAssert
        (inv channelA.peek.opcode.isAcquire)
        "makeTLRAM only allow PutData and Get requests"
      let isPut = channelA.peek.opcode.isPutData

      let addr = (channelA.peek.address - lowerBound) .>>. laneLogSize
      let sz = size.val === 0 ? (1 .<<. channelA.peek.size, size.val)
      let idx = size.val === 0 ? (truncate addr, index.val)
      let msb :: Bit (32 - 23) = truncateLSBCast addr

      if isPut then do
        when (msb === 0 .&&. channelA.peek.mask =!= 0) do
          inputs.enq (idx, channelA.peek.lane, channelA.peek.mask)
          display "write sdram at 0x" (formatHex 0 channelA.peek.address)
      else do
        inputs.enq (idx, 0, 0)
        display "read sdram"

      size <== sz .>. laneSize ? (sz - laneSize, 0)
      index <== idx + 1

      when (isPut .||. sz .<=. laneSize) do
        channelA.consume

      when (inv isPut .||. sz .<=. laneSize) do
        queue.enq
          ChannelD
            { opcode= isPut ? (tag #AccessAck (), tag #AccessAckData ())
            , source= channelA.peek.source
            , size= channelA.peek.size
            , lane= dontCare
            , sink }

    let isPut = queue.first.opcode.isAccessAck
    when (queue.canDeq .&&. channelD.canPut .&&. isPut) do
      channelD.put queue.first
      queue.deq

    when (queue.canDeq .&&. channelD.canPut .&&. inv isPut .&&. outputs.canPeek) do
      channelD.put (queue.first{lane= outputs.peek} :: ChannelD p)
      outputs.consume
      queue.deq

  return
    ( fabric
    , TLSlave
        { channelA= toSink queueA
        , channelB= nullSource
        , channelC= nullSink
        , channelD= toSource queueD
        , channelE= nullSink } )

makeCPU :: Bool -> Bit 1 -> Module (Bit 1, Bit 8, SpiFabric, TLMaster TLConfig', VgaFabric)
makeCPU enableGpu rx = mdo
  (tx, uartInterrupt, leds, uartMmio) <- makeUartMmio 217 rx
  (spi, spiMmio) <- makeSpiMmio 0x10001000

  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ x ->
              select
                [ x .<. 0x70000000 --> 1
                , x .>=. 0x80000000 --> 0
                , 0x70000000 .<=. x .&&. x .<. 0x80000000 --> 2]
          , rootSink= \ x -> 0
          , rootSource= \ x ->
              select
                [ x === 0 --> 0
                , x === 1 --> 1
                , x === 2 --> 1
                , x === 3 --> 0
                , x === 4 --> 1
                , x === 5 --> 2
                , x === 6 --> 3 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master0,master1,master2], [slave0,slave1,slave2,slave3]) <-
    withName "xbar" $ makeTLXBar @3 @4 @TLConfig xbarconfig

  withName "xbar" $ makeConnection master0 slave
  withName "xbar" $ makeConnection imaster0 slave0
  withName "xbar" $ makeConnection dmaster0 slave1

  (clintMmio, clint) <- withName "clint" $ makeClint @TLConfig 2 0x2000000
  clintSlave <- makeTLMmio @TLConfig 1 (clintMmio ++ uartMmio ++ spiMmio)
  withName "clint" $ makeConnection master1 clintSlave

  makeConnection master2 vgaSlave
  (vgaFabric, vgaSlave) <- withName "vga" $ makeVga @TLConfig 0x70000000 2

  let systemInputs0 =
        SystemInputs
          { softwareInterrupt= clint.softwareInterrupt
          , timerInterrupt= clint.timerInterrupt!(0 :: Int)
          , externalInterrupt= uartInterrupt }

  let systemInputs1 =
        SystemInputs
          { softwareInterrupt= clint.softwareInterrupt
          , timerInterrupt= clint.timerInterrupt!(1 :: Int)
          , externalInterrupt= uartInterrupt }

  let coreconfig0 =
        CoreConfig
          { fetchSource= 0
          , dataSource= 1
          , mmioSource= 2
          , itlbSource= 3
          , dtlbSource= 4
          , hartId= 0 }
  (imaster0, dmaster0) <- withName "core0" $ makeCore coreconfig0 systemInputs0

  let gpuBaseHartid :: Bit 32 = 1
  let gpuInstSource :: Bit 8 = 5
  let gpuDataSource :: Bit 8 = 6
  when enableGpu do
    gpuResets <- makeQueue
    always do
      when (delay true false) do
        gpuResets.enq 0x80000000

    (imaster1, dmaster1) <-
      withName "gpu" $ makeSimtCore @TLConfig
        gpuBaseHartid
        (toSource gpuResets)
        gpuInstSource
        gpuDataSource

    withName "xbar" $ makeConnection imaster1 slave2
    withName "xbar" $ makeConnection dmaster1 slave3

  let bconfig =
        BroadcastConfig
          { sources=
              [ coreconfig0.fetchSource
              , coreconfig0.dataSource ]
              ++ if enableGpu then
                  [ gpuInstSource
                  , gpuDataSource ]
                else
                  []
          , logSize= 6
          , baseSink= 0 }
  (slave, uncoherentMaster) <- withName "broadcast" $ makeBroadcast @TLConfig bconfig

  return (tx, leds, spi, uncoherentMaster, vgaFabric)

type RomLogSize = 15
type SimRomLogSize = 24

makeUlx3s :: Bool -> Bit 1 -> Module (Bit 1, Bit 8, SpiFabric, SdramFabric, VgaFabric)
makeUlx3s enableGpu rx = mdo
  (tx, leds, spi, master, vga) <- makeCPU enableGpu rx

  let sdramBase :: Bit 32 = 0x80000000 + 4 * lit (2 ^ valueOf @RomLogSize)
  let stacksBase :: Bit 32 = 0x8F000000

  let sramconfig =
        TLRAMConfig
          { fileName= Just "Mem.hex"
          , lowerBound= 0x80000000
          , bypassChannelA= False
          , bypassChannelD= False
          , sink= 1 }

  let xbarconfig =
        XBarConfig
          { bce= False
          , rootAddr= \ x ->
              if enableGpu then
                select
                  [ x .<. sdramBase --> 1
                  , x .>=. sdramBase .&&. x .<. stacksBase --> 0
                  , x .>=. stacksBase --> 2 ]
              else
                select
                  [ x .<. sdramBase --> 1
                  , x .>=. sdramBase --> 0 ]
          , rootSink= \ x -> 0
          , rootSource= \ x -> 0
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  makeConnection master slave

  ([masterSdram, masterSram, masterStacks], [slave]) <-
    withName "xbar" $ makeTLXBar @3 @1 @TLConfig' xbarconfig

  when enableGpu do
    slaveStacks <- makeGpuStacks @TLConfig' 0
    makeConnection masterStacks slaveStacks

  (fabric, slaveSdram) <- withName "sdram" $ makeTLSdram @TLConfig' 0 sdramBase

  slaveSram <- withName "sram" $ makeTLRAM @RomLogSize @TLConfig' sramconfig

  makeConnection masterSdram slaveSdram
  makeConnection masterSram slaveSram

  return (tx, leds, spi, fabric, vga)


makeTestCore :: Bool -> Bit 1 -> Module (Bit 1, Bit 8)
makeTestCore enableGpu rx = mdo
  (tx, leds, spi, master, vga) <- makeCPU enableGpu rx

  if enableGpu then mdo
    let stacksBase :: Bit 32 = 0x8F000000

    let xbarconfig =
          XBarConfig
            { bce= False
            , rootAddr= \ x -> x .<. stacksBase ? (0, 1)
            , rootSink= \ x -> 0
            , rootSource= \ x -> 0
            , sizeChannelA= 2
            , sizeChannelB= 2
            , sizeChannelC= 2
            , sizeChannelD= 2
            , sizeChannelE= 2 }

    makeConnection master slave

    ([masterSram, masterStacks], [slave]) <-
      withName "xbar" $ makeTLXBar @2 @1 @TLConfig' xbarconfig

    slaveStacks <- makeGpuStacks @TLConfig' 0

    let sramconfig =
          TLRAMConfig
            { fileName= Just "Mem.hex"
            , lowerBound= 0x80000000
            , bypassChannelA= False
            , bypassChannelD= False
            , sink= 1 }
    slaveSram <- withName "sram" $ makeTLRAM @SimRomLogSize @TLConfig' sramconfig

    makeConnection masterStacks slaveStacks
    makeConnection masterSram slaveSram

    return (tx, leds)
  else mdo
    let sramconfig =
          TLRAMConfig
            { fileName= Just "Mem.hex"
            , lowerBound= 0x80000000
            , bypassChannelA= False
            , bypassChannelD= False
            , sink= 1 }
    slaveSram <- withName "sram" $ makeTLRAM @SimRomLogSize @TLConfig' sramconfig

    makeConnection master slaveSram

    return (tx, leds)
