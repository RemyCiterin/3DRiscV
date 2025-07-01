module TileLink.Mmio
  ( Mmio(..)
  , makeTLMmio
  , readOnlyMmio
  , writeOnlyMmio
  , regToMmio
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

import Blarney.Utils

-- - `read` take the mask as argument to allow reads with effect
--   with a granularity of one byte
data Mmio p =
  Mmio
    { address :: Bit (AddrWidth p)
    , read :: Bit (LaneWidth p) -> Action (Bit (8 * LaneWidth p))
    , write :: Bit (8 * LaneWidth p) -> Bit (LaneWidth p) -> Action () }

readOnlyMmio :: forall p. Bit (AddrWidth p) -> Bit (8 * LaneWidth p) -> Mmio p
readOnlyMmio address read = Mmio { address, read= const (pure read), write= \ _ _ -> pure () }

writeOnlyMmio :: forall p.
  (KnownTLParams p)
    => Bit (AddrWidth p) -> (Bit (8 * LaneWidth p) -> Bit (LaneWidth p) -> Action ()) -> Mmio p
writeOnlyMmio address write = Mmio { address, read= const (pure dontCare), write }

regToMmio :: forall p.
  (KnownTLParams p)
    => Bit (AddrWidth p) -> Reg (Bit (8 * LaneWidth p)) -> Mmio p
regToMmio address r =
  Mmio
    { address
    , read= const (pure r.val)
    , write= \ value mask -> r <== mergeBE value r.val mask }

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
            x <- mmio.read channelA.peek.mask
            lane <== x
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
