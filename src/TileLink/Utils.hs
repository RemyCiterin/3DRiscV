module TileLink.Utils where

import TileLink.Types
import Blarney
import Blarney.Connectable
import Blarney.SourceSink
import Blarney.Queue
import Blarney.Ehr
import Blarney.Arbiter
import Blarney.TaggedUnion
import Data.Proxy

instance Connectable (TLMaster p) (TLSlave p) where
  makeConnection master slave = do
    makeConnection master.channelA slave.channelA
    makeConnection slave.channelB master.channelB
    makeConnection master.channelC slave.channelC
    makeConnection slave.channelD master.channelD
    makeConnection master.channelE slave.channelE

hasDataA :: OpcodeA -> Bit 1
hasDataA opcode =
  opcode `is` #Put

hasDataB :: OpcodeB -> Bit 1
hasDataB opcode = false

hasDataC :: OpcodeC -> Bit 1
hasDataC opcode =
  opcode `is` #ProbeAckData .||. opcode `is` #ReleaseData

hasDataD :: OpcodeD -> Bit 1
hasDataD opcode =
  opcode `is` #GrantData .||. opcode `is` #AccessAckData

-- A source with meta informations about the size of the remaining lanes,
-- and tell if the message is the last one of the burst. The offset is the
-- difference between the base address of the burst and the address of the
-- current lane of data
data MetaSource c =
  MetaSource
    { source :: Source c
    , offset :: TLSize
    , size :: TLSize
    , last :: Bit 1
    , first :: Bit 1 }

makeMetaSource :: forall channel sw. (Bits channel, KnownNat sw) =>
  (channel -> Bit sw) ->
  (channel -> Bit 1) ->
  Source channel -> Integer ->
  Module (MetaSource channel)
makeMetaSource getSize hasData source laneSize = do
  first :: Reg (Bit 1) <- makeReg true
  sizeReg :: Reg TLSize <- makeReg 0

  let msg = source.peek
  let size = first.val ? (1 .<<. getSize msg, sizeReg.val)
  let last = size .<. fromInteger laneSize .||. inv (hasData msg)

  return
    MetaSource
      { source=
          Source
            { canPeek= source.canPeek
            , peek= source.peek
            , consume= do
                source.consume
                sizeReg <== size - fromInteger laneSize
                first <== last
            }
      , offset= size - (1 .<<. getSize msg)
      , first= first.val
      , size
      , last }

makeMetaSourceA :: forall p.
  KnownTLParams p => Source (ChannelA p) -> Module (MetaSource (ChannelA p))
makeMetaSourceA source = do
  let laneSize :: Integer = toInteger (valueOf @(LaneWidth p))
  makeMetaSource (\ msg -> msg.size) (\ msg -> hasDataA msg.opcode) source laneSize

makeMetaSourceB :: forall p.
  KnownTLParams p => Source (ChannelB p) -> Module (MetaSource (ChannelB p))
makeMetaSourceB source = do
  let laneSize :: Integer = toInteger (valueOf @(LaneWidth p))
  makeMetaSource (\ msg -> msg.size) (\ msg -> hasDataB msg.opcode) source laneSize

makeMetaSourceC :: forall p.
  KnownTLParams p => Source (ChannelC p) -> Module (MetaSource (ChannelC p))
makeMetaSourceC source = do
  let laneSize :: Integer = toInteger (valueOf @(LaneWidth p))
  makeMetaSource (\ msg -> msg.size) (\ msg -> hasDataC msg.opcode) source laneSize

makeMetaSourceD :: forall p.
  KnownTLParams p => Source (ChannelD p) -> Module (MetaSource (ChannelD p))
makeMetaSourceD source = do
  let laneSize :: Integer = toInteger (valueOf @(LaneWidth p))
  makeMetaSource (\ msg -> msg.size) (\ msg -> hasDataD msg.opcode) source laneSize

getLaneMask :: forall p.
  (KnownTLParams p)
    => Bit (AddrWidth p)
    -> Bit (SizeWidth p)
    -> Bit (LaneWidth p)
getLaneMask address logSize =
  liftNat (log2 (valueOf @(LaneWidth p))) $ \ (_ :: Proxy sz) ->
    let offset :: Bit sz = truncateCast address in
    let size :: TLSize = 1 .<<. logSize in
    ((1 .<<. size) - 1) .<<. offset


-- Transform a sink into multiple shared sink, with a synchronisation such that
-- only one actor can put into the sink per cycle
makeSharedSink ::
  Int -> Sink t -> Module [Sink t]
makeSharedSink size sink = do
  doPut :: [Wire (Bit 1)] <- replicateM size (makeWire false)

  let canPut i =
        if i == 0
           then sink.canPut
           else inv (doPut!(i-1)).val .&&. canPut (i-1)

  return
    [
      Sink
        { canPut= canPut i
        , put= \x -> do
            (doPut!i) <== true
            sink.put x }
    | i <- [0..size-1]]

-- Transform a source into multiple shared sourecs, with a synchronisation such that
-- only one actor can consume from this source per cycle
makeSharedSource ::
  Int -> Source t -> Module [Source t]
makeSharedSource size source = do
  doPeek :: [Wire (Bit 1)] <- replicateM size (makeWire false)

  let canPeek i =
        if i == 0
           then source.canPeek
           else inv (doPeek!(i-1)).val .&&. canPeek (i-1)

  return
    [
      Source
        { canPeek= canPeek i
        , peek= source.peek
        , consume= do
            source.consume
            (doPeek!i) <== true}
    | i <- [0..size-1]]
