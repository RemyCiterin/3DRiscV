module TileLink.Broadcast where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.ADT
import Blarney.Arbiter

import TileLink.Utils
import TileLink.Types

import Blarney.Option

import Data.List as List

data BroadcastConfig p =
  BroadcastConfig
    { sources :: [Bit (SourceWidth p)]
    , fileName :: Maybe String
    , sink :: Bit (SinkWidth p)
    , lowerBound :: Bit (AddrWidth p) }

makeBroadcast :: forall iw p.
  (KnownNat iw, KnownTLParams p, KnownNat (AddrWidth p - iw), iw <= AddrWidth p)
    => BroadcastConfig p -> Module (TLSlave p)
makeBroadcast config = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneLogSize :: TLSize = constant $ toInteger $ log2 $ valueOf @(LaneWidth p)

  let numSources = length config.sources

  queueA :: Queue (ChannelA p) <- makeQueue
  queueB :: Queue (ChannelB p) <- makeQueue
  queueC :: Queue (ChannelC p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue
  queueE :: Queue (ChannelE p) <- makeQueue

  metaC :: MetaSource (ChannelC p) <- makeMetaSourceC @p (toSource queueC)
  let channelC = metaC.source

  let acquire = queueA.first

  queue :: Queue (ChannelD p) <- makePipelineQueue 1

  ram :: RAMBE iw (LaneWidth p) <-
    case config.fileName of
      Nothing -> makeRAMBE
      Just name -> makeRAMInitBE name

  -- acquire state
  sizeA :: Reg TLSize <- makeReg 0
  indexA :: Reg (Bit iw) <- makeReg dontCare
  validA :: Reg (Bit 1) <- makeReg false

  indexC :: Reg (Bit iw) <- makeReg dontCare

  -- need probe
  needProbe :: [Reg (Bit 1)] <- replicateM numSources (makeReg false)
  probeAck :: [Reg (Bit 1)] <- replicateM numSources (makeReg false)

  let sourceOH =
        let blocked = List.inits [inv need.val | need <- needProbe] in
        [x.val .&&. andList y | (x,y) <- zip needProbe blocked]

  let source =
        select [oh --> s | (s,oh) <- zip config.sources sourceOH]

  let canGrant :: Bit 1 = andList [ack.val | ack <- probeAck]

  let translate :: Bit (AddrWidth p) -> Option (Bit iw) = \ a ->
        let addr = (a - config.lowerBound) .>>. laneLogSize in
        let msb :: Bit (AddrWidth p - iw) = truncateLSBCast addr in
        msb === 0 ? (some (truncate addr), none)


  always do
    when (queue.canDeq .&&. queueD.notFull) do
      queueD.enq (queue.first{lane=ram.outBE} :: ChannelD p)
      queue.deq

    when (inv validA.val .&&. queueA.canDeq .&&. acquire.opcode `isTagged` #AcquireBlock) do
      --sequence_
      --  [need <== src =!= acquire.source | (need,src) <- zip needProbe config.sources]
      --sequence_
      --  [ack <== src === acquire.source | (ack,src) <- zip probeAck config.sources]
      sequence_
        [need <== true | need <- needProbe]
      indexA <== (translate acquire.address).val
      sizeA <== 1 .<<. acquire.size
      validA <== true

      display "Slave: " acquire

    when (validA.val .&&. sizeA.val === 0 .&&. queueE.canDeq .&&. queueE.first.sink === config.sink) do
      sequence_ [ack <== false | ack <- probeAck]
      display "Slave: " queueE.first
      validA <== false
      queueA.deq
      queueE.deq

    when (validA.val .&&. orList [need.val | need <- needProbe] .&&. queueB.notFull) do
      queueB.enq
        ChannelB
          { opcode= tag #ProbeBlock (item #N)
          , address= acquire.address
          , size= acquire.size
          , source }
      sequence_ [when oh (need <== false) | (oh,need) <- zip sourceOH needProbe]
      --display "send probe request to source " source

    when (channelC.canPeek .&&. inv canGrant .&&. queueD.notFull) do
      display "Slave: " channelC.peek
      channelC.consume

      when (hasDataC channelC.peek.opcode .&&. (translate channelC.peek.address).valid) do
        let index = metaC.first ? ((translate channelC.peek.address).val, indexC.val)
        ram.storeBE index ones channelC.peek.lane
        indexC <== index + 1

      when (metaC.last) do
        let isProbeAck = channelC.peek.opcode `isTagged` #ProbeAck
        let isProbeAckData = channelC.peek.opcode `isTagged` #ProbeAckData
        if (isProbeAck .||. isProbeAckData) then do
          sequence_
            [when (src === channelC.peek.source) (ack <== true)
              | (src,ack) <- zip config.sources probeAck]
        else do
          queueD.enq
            ChannelD
              { opcode= item #ReleaseAck
              , size= channelC.peek.size
              , source= channelC.peek.source
              , sink= config.sink
              , lane= dontCare }

    when (canGrant .&&. sizeA.val =!= 0 .&&. queue.notFull) do
      queue.enq
        ChannelD
          { opcode= tag #GrantData (item #T)
          , source= acquire.source
          , size= acquire.size
          , sink= config.sink
          , lane= dontCare }

      sizeA <== sizeA.val .>. laneSize ? (sizeA.val - laneSize, 0)
      indexA <== indexA.val + 1
      ram.loadBE indexA.val

  return
    TLSlave
      { channelA= toSink queueA
      , channelB= toSource queueB
      , channelC= toSink queueC
      , channelD= toSource queueD
      , channelE= toSink queueE }
