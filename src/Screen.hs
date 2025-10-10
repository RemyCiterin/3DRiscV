module Screen where

import Blarney
import Blarney.ADT
import Blarney.Queue
import Blarney.SourceSink

import TileLink

data VgaFabric =
  VgaFabric
    { hsync :: Bit 1
    , vsync :: Bit 1
    , blank :: Bit 1
    , red :: Bit 8
    , green :: Bit 8
    , blue :: Bit 8 }
  deriving(Bits, Generic, Interface)

makeVga :: forall p.
  ( KnownTLParams p
  , AddrWidth p ~ 32
  , LaneWidth p ~ 4 )
    => Bit 32 -> Bit (SinkWidth p) -> Module (VgaFabric, TLSlave p)
makeVga lowerBound sink = do
  ---------------------------------------------------------------------------
  -- TileLink Slave
  ---------------------------------------------------------------------------
  let laneSize :: TLSize = 4
  let laneLogSize :: TLSize = 2
  queueA :: Queue (ChannelA p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue
  queue :: Queue (Bit 32, Bit 4, Bit 32) <- makeQueue

  let slave =
        TLSlave
          { channelA= toSink queueA
          , channelB= nullSource
          , channelC= nullSink
          , channelD= toSource queueD
          , channelE= nullSink }

  let channelD = toSink queueD
  let channelA = toSource queueA
  size :: Reg TLSize <- makeReg 0
  index :: Reg (Bit iw) <- makeReg dontCare

  always do
    when (channelA.canPeek .&&. channelD.canPut .&&. queue.notFull) do
      dynamicAssert
        (inv channelA.peek.opcode.isAcquire)
        "makeVga only allow PutData and Get requests"
      let isPut = channelA.peek.opcode.isPutData

      let addr = (channelA.peek.address - lowerBound) .>>. laneLogSize
      let sz = size.val === 0 ? (1 .<<. channelA.peek.size, size.val)
      let idx = size.val === 0 ? (truncate addr, index.val)

      when (isPut) do
        queue.enq (idx, channelA.peek.mask, channelA.peek.lane)

      size <== sz .>. laneSize ? (sz - laneSize, 0)
      index <== idx + 1

      when (isPut .||. sz .<=. laneSize) do
        channelA.consume

      when (inv isPut .||. sz .<=. laneSize) do
        channelD.put
          ChannelD
            { opcode= isPut ? (tag #AccessAck (), tag #AccessAckData ())
            , source= channelA.peek.source
            , size= channelA.peek.size
            , lane= dontCare
            , sink= sink }


  ---------------------------------------------------------------------------
  -- VGA driver
  ---------------------------------------------------------------------------
  let hwidth :: Integer = 640
  let vwidth :: Integer = 480

  let hsync_front_porch :: Integer = 16
  let hsync_pulse_width :: Integer = 96
  let hsync_back_porch :: Integer = 48

  let vsync_front_porch :: Integer = 11
  let vsync_pulse_width :: Integer = 2
  let vsync_back_porch :: Integer = 31

  let hframe = hwidth + hsync_back_porch + hsync_pulse_width + hsync_front_porch
  let vframe = vwidth + vsync_back_porch + vsync_pulse_width + vsync_front_porch

  let xmax = hwidth
  let ymax = vwidth

  palette :: RAM (Bit 8) (Bit 24) <- makeRAMInit "Palette.hex"
  bram :: RAMBE 13 4 <- makeDualRAMBE

  fabric_addr :: Reg (Bit 26) <- makeReg 0
  hpos :: Reg (Bit 13) <- makeReg 0
  vpos :: Reg (Bit 13) <- makeReg 0

  always do
    when queue.canDeq do
      let (addr, mask, lane) = queue.first
      let (msb, lsb) = split addr
      queue.deq

      when (msb === 0) do
        bram.storeBE lsb mask lane

  let next_fabric_addr :: Bit 26 =
        let h :: Bit 26 = zeroExtend (hpos.val .>>. (1::Bit 2)) in
        let v :: Bit 26 = zeroExtend (vpos.val .>>. (1::Bit 2)) in
        let r = h + v * lit (div xmax 2) in
        r .>=. lit (div (xmax * ymax) 4) ? (0,r)

  let fabric_response :: Bit 8 =
        select
          [ slice @1 @0 fabric_addr.val === 0b00 --> slice @7 @0 bram.outBE
          , slice @1 @0 fabric_addr.val === 0b01 --> slice @15 @8 bram.outBE
          , slice @1 @0 fabric_addr.val === 0b10 --> slice @23 @16 bram.outBE
          , slice @1 @0 fabric_addr.val === 0b11 --> slice @31 @24 bram.outBE ]

  always do
    bram.loadBE (truncate (next_fabric_addr .>>. (2::Bit 2)))
    palette.load fabric_response

    let next_hpos = (hpos.val+1 .>=. lit hframe) ? (0, hpos.val+1)
    let next_vpos =
          (hpos.val+1 .>=. lit hframe) ?
            ((vpos.val+1 .>=. lit vframe) ? (0, vpos.val+1), vpos.val)

    fabric_addr <== next_fabric_addr
    hpos <== next_hpos
    vpos <== next_vpos

  let fabric =
        VgaFabric
          { hsync=
            hpos.val .<. lit (hwidth + hsync_front_porch) .||.
            hpos.val .>=. lit (hframe - hsync_back_porch)
          , vsync=
            vpos.val .<. lit (vwidth + vsync_front_porch) .||.
            vpos.val .>=. lit (vframe - vsync_back_porch)
          , blank=
            hpos.val .>=. lit hwidth .||. vpos.val .>=. lit vwidth
          , red=
            slice @23 @16 palette.out
          , green=
            slice @15 @8 palette.out
          , blue=
            slice @7 @0 palette.out }

  return (fabric, slave)
