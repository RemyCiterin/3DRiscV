module TileLink.BusWidth
  ( makeIncreaseWidth
  , makeDecreaseWidth
  ) where

import Blarney
import Blarney.Vector (Vec, fromList, toList)
import qualified Blarney.Vector as Vec
import Blarney.TypeFamilies
import Blarney.Connectable
import Blarney.SourceSink
import Blarney.Sharing
import Blarney.Queue

import TileLink.Types
import TileLink.Utils

import Data.Proxy

type Token n = Bit (Log2Ceil n)

makeIncreaseWidthChannelA :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Sink (ChannelA p2) -> Source (ChannelA p1) -> Module ()
makeIncreaseWidthChannelA sink source' = do
  meta <- makeMetaSourceA @p1 source'
  let source = meta.source
  let msg = source.peek

  laneReg :: Reg (Vec n (Bit (8 * LaneWidth p1))) <- makeReg dontCare
  maskReg :: Reg (Vec n (Bit (LaneWidth p1))) <- makeReg dontCare
  index :: Reg (Token n) <- makeReg 0

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  always do
    when (source.canPeek .&&. sink.canPut) do
      let pos :: Token n = unsafeSlice (high,low) meta.offset
      let mask :: Vec n (Bit (LaneWidth p1)) =
            fromList
              [pos === lit i ? (msg.mask, maskReg.val!i)
                | i <- [0..toInteger $ valueOf @n - 1]]
      let lane :: Vec n (Bit (8 * LaneWidth p1)) =
            fromList
              [pos === lit i ? (msg.lane, laneReg.val!i)
                | i <- [0..toInteger $ valueOf @n - 1]]

      when (index.val === ones .||. meta.last) do
        sink.put
          ChannelA
            { size= msg.size
            , mask= pack(mask)
            , lane= pack(lane)
            , opcode= msg.opcode
            , source= msg.source
            , address= msg.address }

      laneReg <== (meta.last .||. index.val === ones) ? (Vec.replicate 0, lane)
      maskReg <== (meta.last .||. index.val === ones) ? (Vec.replicate 0, mask)
      index <== (meta.last .||. index.val === ones) ? (0, index.val + 1)
      source.consume

makeIncreaseWidthChannelC :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Sink (ChannelC p2) -> Source (ChannelC p1) -> Module ()
makeIncreaseWidthChannelC sink source' = do
  meta <- makeMetaSourceC @p1 source'
  let source = meta.source
  let msg = source.peek

  laneReg :: Reg (Vec n (Bit (8 * LaneWidth p1))) <- makeReg dontCare
  index :: Reg (Token n) <- makeReg 0

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  always do
    when (source.canPeek .&&. sink.canPut) do
      let pos :: Token n = unsafeSlice (high,low) meta.offset
      let lane :: Vec n (Bit (8 * LaneWidth p1)) =
            fromList
              [pos === lit i ? (msg.lane, laneReg.val!i)
                | i <- [0..toInteger $ valueOf @n - 1]]

      when (index.val === ones .||. meta.last) do
        sink.put
          ChannelC
            { size= msg.size
            , lane= pack(lane)
            , opcode= msg.opcode
            , source= msg.source
            , address= msg.address }

      laneReg <== (meta.last .||. index.val === ones) ? (Vec.replicate 0, lane)
      index <== (meta.last .||. index.val === ones) ? (0, index.val + 1)
      source.consume

makeIncreaseWidthChannelD :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Sink (ChannelA p1) -> Sink (ChannelD p2) -> Source (ChannelD p1) -> Module (Sink (ChannelA p1))
makeIncreaseWidthChannelD sinkA sink source' = do
  meta <- makeMetaSourceD @p1 source'
  let source = meta.source
  let msg = source.peek

  positions :: RegFile (Bit (SourceWidth p1)) (Token n) <- makeRegFile
  laneReg :: Reg (Vec n (Bit (8 * LaneWidth p1))) <- makeReg dontCare
  index :: Reg (Token n) <- makeReg 0

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  always do
    when (source.canPeek .&&. sink.canPut) do
      let pos :: Token n = meta.first ? (positions!msg.source, index.val)
      let lane :: Vec n (Bit (8 * LaneWidth p1)) =
            fromList
              [pos === lit i ? (msg.lane, laneReg.val!i)
                | i <- [0..toInteger $ valueOf @n - 1]]

      when (index.val === ones .||. meta.last) do
        sink.put
          ChannelD
            { size= msg.size
            , sink= msg.sink
            , lane= pack(lane)
            , opcode= msg.opcode
            , source= msg.source }

      laneReg <== (meta.last .||. index.val === ones) ? (Vec.replicate 0, lane)
      index <== (meta.last .||. index.val === ones) ? (0, index.val + 1)
      source.consume

  return
    Sink
      { canPut= sinkA.canPut
      , put= \ x -> do
          let pos :: Token n = unsafeSlice (high,low) x.address
          positions.update (x.source) pos
          sinkA.put x }

makeDecreaseWidthChannelA :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Sink (ChannelA p1) -> Source (ChannelA p2) -> Module ()
makeDecreaseWidthChannelA sink source = do
  let msg = source.peek

  let mask :: Vec n (Bit (LaneWidth p1)) = unpack msg.mask
  let lane :: Vec n (Bit (8 * LaneWidth p1)) = unpack msg.lane

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  indexReg :: Reg (Token n) <- makeReg 0

  let size :: TLSize = 1 .<<. msg.size
  let firstIndex :: Token n = unsafeSlice (high,low) msg.address
  let length :: Token n =
        (hasDataA msg.opcode .&&. size .>. lit (toInteger (valueOf @(LaneWidth p1)))) ?
          (unsafeSlice (high,low) size, 1)

  first :: Reg (Bit 1) <- makeReg true

  always do
    when (sink.canPut .&&. source.canPeek) do
      let index = first.val ? (firstIndex, indexReg.val)
      let last = index+1 === firstIndex + length

      sink.put
        ChannelA
          { opcode= msg.opcode
          , address= msg.address
          , source= msg.source
          , size= msg.size
          , lane= lane!index
          , mask= mask!index}

      first <== last
      indexReg <== index + 1
      when last source.consume

makeDecreaseWidthChannelC :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Sink (ChannelC p1) -> Source (ChannelC p2) -> Module ()
makeDecreaseWidthChannelC sink source = do
  let msg = source.peek

  let lane :: Vec n (Bit (8 * LaneWidth p1)) = unpack msg.lane

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  indexReg :: Reg (Token n) <- makeReg 0

  let size :: TLSize = 1 .<<. msg.size
  let firstIndex :: Token n = unsafeSlice (high,low) msg.address
  let length :: Token n =
        (hasDataC msg.opcode .&&. size .>. lit (toInteger (valueOf @(LaneWidth p1)))) ?
          (unsafeSlice (high,low) size, 1)

  first :: Reg (Bit 1) <- makeReg true

  always do
    when (sink.canPut .&&. source.canPeek) do
      let index = first.val ? (firstIndex, indexReg.val)
      let last = index+1 === firstIndex + length

      sink.put
        ChannelC
          { opcode= msg.opcode
          , address= msg.address
          , source= msg.source
          , size= msg.size
          , lane= lane!index }

      first <== last
      indexReg <== index + 1
      when last source.consume


makeDecreaseWidthChannelD :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Source (ChannelA p1) -> Sink (ChannelD p1) -> Source (ChannelD p2) -> Module ()
makeDecreaseWidthChannelD sourceA sink source = do
  let msg = source.peek

  let low = log2 (valueOf @(LaneWidth p1))
  let high = low + log2 (valueOf @n) - 1

  positions :: RegFile (Bit (SourceWidth p1)) (Token n) <- makeRegFile
  always do
    when sourceA.canPeek do
      let pos :: Token n = unsafeSlice (high,low) sourceA.peek.address
      positions.update sourceA.peek.source pos

  let lane :: Vec n (Bit (8 * LaneWidth p1)) = unpack msg.lane

  indexReg :: Reg (Token n) <- makeReg 0

  let size :: TLSize = 1 .<<. msg.size
  let firstIndex :: Token n = positions!msg.source
  let length :: Token n =
        (hasDataD msg.opcode .&&. size .>. lit (toInteger (valueOf @(LaneWidth p1)))) ?
          (unsafeSlice (high,low) size, 1)

  first :: Reg (Bit 1) <- makeReg true

  always do
    when (sink.canPut .&&. source.canPeek) do
      let index = first.val ? (firstIndex, indexReg.val)
      let last = index+1 === firstIndex + length

      sink.put
        ChannelD
          { opcode= msg.opcode
          , source= msg.source
          , size= msg.size
          , sink= msg.sink
          , lane= lane!index }

      first <== last
      indexReg <== index + 1
      when last source.consume

makeIncreaseWidth :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Bool -> TLMaster p1 -> TLSlave p2 -> Module ()
makeIncreaseWidth bce master slave = do
  when bce do
    makeIncreaseWidthChannelC @n @p1 @p2 slave.channelC master.channelC

  makeDecreaseWidthChannelD @n @p1 @p2 master.channelA master.channelD slave.channelD
  makeIncreaseWidthChannelA @n @p1 @p2 slave.channelA master.channelA

  always do
    when bce do
      when (slave.channelB.canPeek .&&. master.channelB.canPut) do
        slave.channelB.consume
        master.channelB.put
          ChannelB
            { address= slave.channelB.peek.address
            , opcode= slave.channelB.peek.opcode
            , source= slave.channelB.peek.source
            , size= slave.channelB.peek.size }

      when (slave.channelE.canPut .&&. master.channelE.canPeek) do
        master.channelE.consume
        slave.channelE.put
          ChannelE
            { sink= master.channelE.peek.sink }


makeDecreaseWidth :: forall n p1 p2.
  ( KnownTLParams p1
  , KnownTLParams p2
  , KnownNat n
  , KnownNat (Log2Ceil n)
  , AddrWidth p2 ~ AddrWidth p1
  , SourceWidth p2 ~ SourceWidth p1
  , SinkWidth p2 ~ SinkWidth p1
  , SizeWidth p2 ~ SizeWidth p1
  , LaneWidth p2 ~ (n * LaneWidth p1)
  , 8 * LaneWidth p2 ~ (n * (8 * LaneWidth p1)))
  => Bool -> TLMaster p2 -> TLSlave p1 -> Module ()
makeDecreaseWidth bce master slave = do
  when bce do
    makeDecreaseWidthChannelC @n @p1 @p2 slave.channelC master.channelC

  chA <- makeIncreaseWidthChannelD @n @p1 @p2 slave.channelA master.channelD slave.channelD
  makeDecreaseWidthChannelA @n @p1 @p2 chA master.channelA

  always do
    when bce do
      when (slave.channelB.canPeek .&&. master.channelB.canPut) do
        slave.channelB.consume
        master.channelB.put
          ChannelB
            { address= slave.channelB.peek.address
            , opcode= slave.channelB.peek.opcode
            , source= slave.channelB.peek.source
            , size= slave.channelB.peek.size }

      when (slave.channelE.canPut .&&. master.channelE.canPeek) do
        master.channelE.consume
        slave.channelE.put
          ChannelE
            { sink= master.channelE.peek.sink }
