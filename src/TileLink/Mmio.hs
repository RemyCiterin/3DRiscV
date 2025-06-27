module TileLink.Mmio
  ( Mmio(..)
  , makeTLMmio
  ) where

import TileLink.Utils
import TileLink.Types

import Blarney
import Blarney.Core.BV
import Blarney.Arbiter
import Blarney.Queue
import Blarney.SourceSink
import Blarney.ADT
import Blarney.Ehr

data Mmio p =
  Mmio
    { address :: Bit (AddrWidth p)
    , read :: Bit (8 * LaneWidth p)
    , write :: Bit (8 * LaneWidth p) -> Bit (LaneWidth p) -> Action () }

-- Generate a RAM controller using a lower bound and a file name
makeTLMmio :: forall p.
  KnownTLParams p
    => Bit (SinkWidth p)
    -> [Mmio p]
    -> Module (TLSlave p)
makeTLMmio sink confs = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneSize' = constant $ toInteger $ valueOf @(LaneWidth p)

  queueA :: Queue (ChannelA p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue

  let channelD = toSink queueD
  let channelA = toSource queueA
  size :: Reg TLSize <- makeReg 0
  address :: Reg (Bit (AddrWidth p)) <- makeReg dontCare

  let align addr = addr .&. inv (laneSize' - 1)
  lane :: Wire (Bit (8 * LaneWidth p)) <- makeWire dontCare

  always do
    when (channelA.canPeek .&&. channelD.canPut) do
      dynamicAssert
        (channelA.peek.opcode `isTagged` #PutData .||. channelA.peek.opcode `isTagged` #Get)
        "makeTLMmio only allow PutData and Get requests"

      let isPut = channelA.peek.opcode `isTagged` #PutData
      let sz = size.val === 0 ? (1 .<<. channelA.peek.size, size.val)
      let addr = size.val === 0 ? (channelA.peek.address, address.val)

      sequence_
        [when (align addr === mmio.address) do
          if (isPut) then do
            mmio.write channelA.peek.lane channelA.peek.mask
          else do
            --display "read " mmio.read " at 0x" (formatHex 0 mmio.address)
            lane <== mmio.read
          | mmio <- confs]

      size <== sz .>. laneSize ? (sz - laneSize, 0)
      address <== align addr + (constant $ toInteger $ valueOf @(LaneWidth p))

      when (isPut .||. sz .<=. laneSize) do
        channelA.consume

      when (inv isPut .||. sz .<=. laneSize) do
        channelD.put
          ChannelD
            { opcode= isPut ? (tag #AccessAck (), tag #AccessAckData ())
            , source= channelA.peek.source
            , size= channelA.peek.size
            , lane= lane.val
            , sink= sink }

  return
    TLSlave
      { channelA= toSink queueA
      , channelB= nullSource
      , channelC= nullSink
      , channelD= toSource queueD
      , channelE= nullSink }
