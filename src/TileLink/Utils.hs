module TileLink.Utils where

import TileLink.Types
import Blarney
import Blarney.Connectable
import Blarney.SourceSink
import Blarney.Queue
import Blarney.Ehr
import Blarney.Arbiter
import Blarney.ADT
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
  opcode.isPutData

hasDataB :: OpcodeB -> Bit 1
hasDataB opcode = false

hasDataC :: OpcodeC -> Bit 1
hasDataC opcode =
  opcode.isProbeAckData .||. opcode.isReleaseData

hasDataD :: OpcodeD -> Bit 1
hasDataD opcode =
  opcode.isGrantData .||. opcode.isAccessAckData

instance KnownNat_ChannelA aw dw sw ow => FShow (ChannelA_Flit aw dw sw ow) where
  fshow msg =
      formatOpcodeA msg.opcode <> fshow "{" <>
      formatCond (hasDataA msg.opcode) formatData <>
      formatCond (inv (hasDataA msg.opcode)) formatNoData <>
      fshow " }"
    where
      address = fshow " address= 0x" <> formatHex 0 msg.address
      source = fshow " source= " <> formatDec 0 msg.source
      size = fshow " size= " <> formatDec 0 ((1 :: TLSize) .<<. msg.size)

      lane = [(unsafeSlice (i*8+7,i*8) msg.lane,unsafeAt i msg.mask) | i <- [0..valueOf @dw - 1]]

      formatLane :: [(Bit 8,Bit 1)] -> Format
      formatLane ((byte,en) : xs) =
        formatLane xs <> formatCond en (formatHex 2 byte) <> formatCond (inv en) (fshow "XX")
      formatLane [] = fshow " lane= 0x"

      sep :: Format
      sep = fshow ","

      formatData = source <> sep <> size <> address <> sep <> formatLane lane
      formatNoData = source <> sep <> size <> address

instance (KnownNat aw, KnownNat sw, KnownNat ow) => FShow (ChannelB_Flit aw sw ow) where
  fshow msg =
      formatOpcodeB msg.opcode <> fshow "{" <>
      formatContent <>
      fshow " }"
    where
      address = fshow " address= 0x" <> formatHex 0 msg.address
      source = fshow " source= " <> formatDec 0 msg.source
      size = fshow " size= " <> formatDec 0 ((1 :: TLSize) .<<. msg.size)

      sep :: Format
      sep = fshow ","

      formatContent = source <> sep <> size <> address

instance KnownNat_ChannelC aw dw sw ow => FShow (ChannelC_Flit aw dw sw ow) where
  fshow msg =
      formatOpcodeC msg.opcode <> fshow "{" <>
      formatCond (hasDataC msg.opcode) formatData <>
      formatCond (inv (hasDataC msg.opcode)) formatNoData <>
      fshow " }"
    where
      address = fshow " address= 0x" <> formatHex 0 msg.address
      source = fshow " source= " <> formatDec 0 msg.source
      size = fshow " size= " <> formatDec 0 ((1 :: TLSize) .<<. msg.size)
      lane = fshow " lane= 0x" <> formatHex 0 msg.lane

      sep :: Format
      sep = fshow ","

      formatData = source <> sep <> size <> address <> sep <> lane
      formatNoData = source <> sep <> size <> address


instance KnownNat_ChannelD dw sw ow iw => FShow (ChannelD_Flit dw sw ow iw) where
  fshow msg =
      formatOpcodeD msg.opcode <> fshow "{" <>
      formatCond (hasDataD msg.opcode) formatData <>
      formatCond (inv (hasDataD msg.opcode)) formatNoData <>
      fshow " }"
    where
      size = fshow " size= " <> formatDec 0 ((1 :: TLSize) .<<. msg.size)
      source = fshow " source= " <> formatDec 0 msg.source
      lane = fshow " lane= 0x" <> formatHex 0 msg.lane
      sink = fshow " sink= " <> formatDec 0 msg.sink

      sep :: Format
      sep = fshow ","

      formatData = source <> sep <> sink <> sep <> size <> sep <> lane
      formatNoData = source <> sep <> sink <> sep <> size

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
  let last = size .<=. fromInteger laneSize .||. inv (hasData msg)

  return
    MetaSource
      { source=
          Source
            { canPeek= source.canPeek
            , peek= source.peek
            , consume= do
                source.consume
                sizeReg <== size .<. fromInteger laneSize ? (0, size - fromInteger laneSize)
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

capTo :: Cap -> TLPerm
capTo cap =
  select
    [ cap `isTagged` #N --> nothing
    , cap `isTagged` #B --> branch
    , cap `isTagged` #T --> trunk ]
