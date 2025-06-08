{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TileLink where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Vector

import Blarney.TaggedUnion

import Data.Proxy

data TLParamsKind =
  TLParams
    Nat -- size width (in bits)
    Nat -- addr width (in bits)
    Nat -- lane width (in bytes)
    Nat -- source id
    Nat -- sink id

type family AddrWidth params where
  AddrWidth (TLParams z a w o i) = a

type family LaneWidth params where
  LaneWidth (TLParams z a w o i) = w

type family SizeWidth params where
  SizeWidth (TLParams z a w o i) = z

type family SourceWidth params where
  SourceWidth (TLParams z a w o i) = o

type family SinkWidth params where
  SinkWidth (TLParams z a w o i) = i

type KnownTLParams (p :: TLParamsKind) =
  ( KnownNat (AddrWidth p)
  , KnownNat (LaneWidth p)
  , KnownNat (8*(LaneWidth p))
  , KnownNat (SizeWidth p)
  , KnownNat (SourceWidth p)
  , KnownNat (SinkWidth p))

-- As TileLink transactions are 4096 bytes masks, 13 bits is enough to represent all the possible
-- sizes
type TLSize = Bit 13

type TLPerm =
  TaggedUnion [
    "Nothing" ::: (),
    "Trunk" ::: (),
    "Dirty" ::: (),
    "Branch" ::: ()
  ]

type Grow =
  TaggedUnion [
    "NtoB" ::: (),
    "NtoT" ::: (),
    "BtoT" ::: ()
  ]

type Reduce =
  TaggedUnion [
    "TtoB" ::: (),
    "TtoN" ::: (),
    "BtoB" ::: (),
    "TtoT" ::: (),
    "BtoN" ::: (),
    "NtoN" ::: ()
  ]

type Cap =
  TaggedUnion [
    "T" ::: (),
    "B" ::: (),
    "N" ::: ()
  ]

type OpcodeA =
  TaggedUnion [
    "AcquirePerms" ::: Grow,
    "AcquireBlock" ::: Grow,
    "PutData" ::: (),
    "Get" ::: ()
  ]

type OpcodeB =
  TaggedUnion [
    "ProbePerms" ::: Cap,
    "ProbeBlock" ::: Cap
  ]

type OpcodeC =
  TaggedUnion [
    "ProbeAck" ::: Reduce,
    "ProbeAckData" ::: Reduce,
    "Release" ::: Reduce,
    "ReleaseData" ::: Reduce
  ]

type OpcodeD =
  TaggedUnion [
    "Grant" ::: Cap,
    "GrantData" ::: Cap,
    "ReleaseAck" ::: (),
    "AccessAckData" ::: (),
    "AccessAck" ::: ()
  ]

data ChannelA_Flit aw dw sw ow =
  ChannelA
    { opcode :: OpcodeA
    , size :: Bit sw
    , source :: Bit ow
    , address :: Bit aw
    , mask :: Bit dw
    , lane :: Bit (8*dw)}
    deriving(Generic)

type KnownNat_ChannelA aw dw sw ow =
  (KnownNat aw, KnownNat dw, KnownNat (8*dw), KnownNat sw, KnownNat ow)

instance KnownNat_ChannelA aw dw sw ow => Bits (ChannelA_Flit aw dw sw ow)

data ChannelB_Flit aw sw ow =
  ChannelB
    { opcode :: OpcodeB
    , size :: Bit sw
    , source :: Bit ow
    , address :: Bit aw}
    deriving(Generic, Bits)

data ChannelC_Flit aw dw sw ow =
  ChannelC
    { opcode :: OpcodeC
    , size :: Bit sw
    , source :: Bit ow
    , address :: Bit aw
    , lane :: Bit (8*dw)}
    deriving(Generic)

type KnownNat_ChannelC aw dw sw ow =
  (KnownNat aw, KnownNat (8*dw), KnownNat sw, KnownNat ow)

instance KnownNat_ChannelC aw dw sw ow => Bits (ChannelC_Flit aw dw sw ow)

data ChannelD_Flit aw dw sw ow iw =
  ChannelD
    { opcode :: OpcodeD
    , size :: Bit sw
    , source :: Bit ow
    , sink :: Bit iw
    , address :: Bit aw
    , lane :: Bit (8*dw)}
    deriving(Generic)

type KnownNat_ChannelD aw dw sw ow iw =
  (KnownNat aw, KnownNat (8*dw), KnownNat sw, KnownNat ow, KnownNat iw)

instance KnownNat_ChannelD aw dw sw ow iw => Bits (ChannelD_Flit aw dw sw ow iw)

data ChannelE_Flit iw =
  ChannelE
    { sink :: Bit iw }
    deriving(Generic, Bits)


type ChannelA (p :: TLParamsKind) =
  ChannelA_Flit (AddrWidth p) (LaneWidth p) (SizeWidth p) (SourceWidth p)

type ChannelB (p :: TLParamsKind) =
  ChannelB_Flit (AddrWidth p) (SizeWidth p) (SourceWidth p)

type ChannelC (p :: TLParamsKind) =
  ChannelC_Flit (AddrWidth p) (LaneWidth p) (SizeWidth p) (SourceWidth p)

type ChannelD (p :: TLParamsKind) =
  ChannelD_Flit (AddrWidth p) (LaneWidth p) (SizeWidth p) (SourceWidth p) (SinkWidth p)

type ChannelE (p :: TLParamsKind) =
  ChannelE_Flit (SinkWidth p)

data TLSlave (p :: TLParamsKind) =
  TLSlave
    { channelA :: Sink (ChannelA p)
    , channelB :: Source (ChannelB p)
    , channelC :: Sink (ChannelC p)
    , channelD :: Source (ChannelD p)
    , channelE :: Sink (ChannelE p)}
    deriving(Generic)

data TLMaster (p :: TLParamsKind) =
  TLMaster
    { channelA :: Source (ChannelA p)
    , channelB :: Sink (ChannelB p)
    , channelC :: Source (ChannelC p)
    , channelD :: Sink (ChannelD p)
    , channelE :: Source (ChannelE p)}
    deriving(Generic)

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
-- and tell if the message is the last one of the burst
data MetaSource c =
  MetaSource
    { source :: Source c
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
      , first= first.val
      , size
      , last }

makeMetaSourceA :: forall p.
  KnownTLParams p => Source (ChannelA p) -> Module (MetaSource (ChannelA p))
makeMetaSourceA source = do
  let laneSize :: Integer = toInteger (valueOf @(LaneWidth p))
  makeMetaSource (\ msg -> msg.size) (\ msg -> hasDataA msg.opcode) source laneSize

splitSinkChannelA ::
  forall p. KnownTLParams p => Int -> Sink (ChannelA p) -> Module [Sink (ChannelA p)]
splitSinkChannelA size sink = do
  doPut :: [Wire (Bit 1)] <- Blarney.replicateM size (makeWire false)

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
