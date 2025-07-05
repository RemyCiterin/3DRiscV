module TileLink.RAM where

import TileLink.Utils
import TileLink.Types
import Blarney
import Blarney.Arbiter
import Blarney.Queue
import Blarney.SourceSink
import Blarney.ADT
import Blarney.Ehr

data TLRAMConfig p =
  TLRAMConfig
    { lowerBound :: Bit (AddrWidth p)
    , fileName :: Maybe String
    , sink :: Bit (SinkWidth p)
    , bypassChannelA :: Bool
    , bypassChannelD :: Bool }

-- Generate a RAM controller using a lower bound and a file name
makeTLRAM :: forall iw p.
  (KnownNat iw, KnownTLParams p, KnownNat (AddrWidth p - iw), iw <= AddrWidth p)
    => TLRAMConfig p -> Module (TLSlave p)
makeTLRAM config = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneLogSize :: TLSize = constant $ toInteger $ log2 $ valueOf @(LaneWidth p)
  ram :: RAMBE iw (LaneWidth p) <-
    case config.fileName of
      Nothing -> makeRAMBE
      Just name -> makeRAMInitBE name

  -- Queue between the stages 1 and 2
  queue :: Queue (ChannelD p) <- makePipelineQueue 1
  queueA :: Queue (ChannelA p) <- if config.bypassChannelA then makeBypassQueue else makeQueue
  queueD :: Queue (ChannelD p) <- if config.bypassChannelD then makeBypassQueue else makeQueue

  let channelD = toSink queueD
  let channelA = toSource queueA
  size :: Reg TLSize <- makeReg 0
  index :: Reg (Bit iw) <- makeReg dontCare

  always do
    when (channelA.canPeek .&&. queue.notFull) do
      dynamicAssert
        (inv channelA.peek.opcode.isAcquire)
        "makeTLRAM only allow PutData and Get requests"
      let isPut = channelA.peek.opcode.isPutData

      let addr = (channelA.peek.address - config.lowerBound) .>>. laneLogSize
      let sz = size.val === 0 ? (1 .<<. channelA.peek.size, size.val)
      let idx = size.val === 0 ? (truncate addr, index.val)
      let msb :: Bit (AddrWidth p - iw) = truncateLSBCast addr

      if isPut then do
        when (msb === 0) do
          ram.storeBE idx channelA.peek.mask channelA.peek.lane
      else do
        ram.loadBE idx

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
            , sink= config.sink }

    when (queue.canDeq .&&. channelD.canPut) do
      channelD.put (queue.first{lane= ram.outBE} :: ChannelD p)
      queue.deq

  return
    TLSlave
      { channelA= toSink queueA
      , channelB= nullSource
      , channelC= nullSink
      , channelD= toSource queueD
      , channelE= nullSink }
