module TileLink.Types where

import Blarney hiding(Vercor)
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.TaggedUnion
import Blarney.Arbiter

import Data.Proxy

data TLParamsKind =
  TLParams
    Nat -- addr width (in bits)
    Nat -- lane width (in bytes)
    Nat -- size width (in bits)
    Nat -- source id
    Nat -- sink id

type family AddrWidth params where
  AddrWidth (TLParams a w z o i) = a

type family LaneWidth params where
  LaneWidth (TLParams a w z o i) = w

type family SizeWidth params where
  SizeWidth (TLParams a w z o i) = z

type family SourceWidth params where
  SourceWidth (TLParams a w z o i) = o

type family SinkWidth params where
  SinkWidth (TLParams a w z o i) = i

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

formatTLPerm :: TLPerm -> Format
formatTLPerm perm =
  formatCond (perm `is` #Nothing) (fshow "Nothing") <>
  formatCond (perm `is` #Branch) (fshow "Branch") <>
  formatCond (perm `is` #Trunk) (fshow "Trunk") <>
  formatCond (perm `is` #Dirty) (fshow "Dirty")

type Grow =
  TaggedUnion [
    "NtoB" ::: (),
    "NtoT" ::: (),
    "BtoT" ::: ()
  ]

formatGrow :: Grow -> Format
formatGrow perm =
  formatCond (perm `is` #NtoB) (fshow "NtoB") <>
  formatCond (perm `is` #NtoT) (fshow "NtoT") <>
  formatCond (perm `is` #BtoT) (fshow "BtoT")

type Reduce =
  TaggedUnion [
    "TtoB" ::: (),
    "TtoN" ::: (),
    "BtoB" ::: (),
    "TtoT" ::: (),
    "BtoN" ::: (),
    "NtoN" ::: ()
  ]

formatReduce :: Reduce -> Format
formatReduce perm =
  formatCond (perm `is` #TtoB) (fshow "TtoB") <>
  formatCond (perm `is` #TtoN) (fshow "TtoN") <>
  formatCond (perm `is` #BtoB) (fshow "BtoB") <>
  formatCond (perm `is` #TtoT) (fshow "TtoT") <>
  formatCond (perm `is` #BtoN) (fshow "BtoN") <>
  formatCond (perm `is` #NtoN) (fshow "NtoN")

type Cap =
  TaggedUnion [
    "T" ::: (),
    "B" ::: (),
    "N" ::: ()
  ]

formatCap :: Cap -> Format
formatCap perm =
  formatCond (perm `is` #T) (fshow "T") <>
  formatCond (perm `is` #B) (fshow "B") <>
  formatCond (perm `is` #N) (fshow "N")

type OpcodeA =
  TaggedUnion [
    "AcquirePerms" ::: Grow,
    "AcquireBlock" ::: Grow,
    "PutData" ::: (),
    "Get" ::: ()
  ]

formatOpcodeA :: OpcodeA -> Format
formatOpcodeA opcode =
  formatCond (opcode `is` #Get) (fshow "Get") <>
  formatCond (opcode `is` #PutData) (fshow "PutData") <>
  formatCond
    (opcode `is` #AcquireBlock)
    (fshow "AcquireBlock<" <> formatGrow (untag #AcquireBlock opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #AcquirePerms)
    (fshow "AcquirePerms<" <> formatGrow (untag #AcquirePerms opcode) <> fshow ">")

type OpcodeB =
  TaggedUnion [
    "ProbePerms" ::: Cap,
    "ProbeBlock" ::: Cap
  ]


formatOpcodeB :: OpcodeB -> Format
formatOpcodeB opcode =
  formatCond
    (opcode `is` #ProbePerms)
    (fshow "ProbePerms<" <> formatCap (untag #ProbePerms opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #ProbeBlock)
    (fshow "ProbeBlock<" <> formatCap (untag #ProbeBlock opcode) <> fshow ">")

type OpcodeC =
  TaggedUnion [
    "ProbeAck" ::: Reduce,
    "ProbeAckData" ::: Reduce,
    "Release" ::: Reduce,
    "ReleaseData" ::: Reduce
  ]

formatOpcodeC :: OpcodeC -> Format
formatOpcodeC opcode =
  formatCond
    (opcode `is` #ProbeAck)
    (fshow "ProbeAck<" <> formatReduce (untag #ProbeAck opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #ProbeAckData)
    (fshow "ProbeAckData<" <> formatReduce (untag #ProbeAckData opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #Release)
    (fshow "Release<" <> formatReduce (untag #Release opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #ReleaseData)
    (fshow "ReleaseData<" <> formatReduce (untag #ReleaseData opcode) <> fshow ">")

type OpcodeD =
  TaggedUnion [
    "Grant" ::: Cap,
    "GrantData" ::: Cap,
    "ReleaseAck" ::: (),
    "AccessAckData" ::: (),
    "AccessAck" ::: ()
  ]

formatOpcodeD :: OpcodeD -> Format
formatOpcodeD opcode =
  formatCond (opcode `is` #ReleaseAck) (fshow "ReleaseAck") <>
  formatCond (opcode `is` #AccessAck) (fshow "AccessAck") <>
  formatCond (opcode `is` #AccessAckData) (fshow "AccessAckData") <>
  formatCond
    (opcode `is` #Grant)
    (fshow "Grant<" <> formatCap (untag #Grant opcode) <> fshow ">") <>
  formatCond
    (opcode `is` #GrantData)
    (fshow "GrantData<" <> formatCap (untag #GrantData opcode) <> fshow ">")

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

data ChannelD_Flit dw sw ow iw =
  ChannelD
    { opcode :: OpcodeD
    , size :: Bit sw
    , source :: Bit ow
    , sink :: Bit iw
    , lane :: Bit (8*dw)}
    deriving(Generic)

type KnownNat_ChannelD dw sw ow iw =
  (KnownNat (8*dw), KnownNat sw, KnownNat ow, KnownNat iw)

instance KnownNat_ChannelD dw sw ow iw => Bits (ChannelD_Flit dw sw ow iw)

data ChannelE_Flit iw =
  ChannelE
    { sink :: Bit iw }
    deriving(Generic, Bits, FShow)


type ChannelA (p :: TLParamsKind) =
  ChannelA_Flit (AddrWidth p) (LaneWidth p) (SizeWidth p) (SourceWidth p)

type ChannelB (p :: TLParamsKind) =
  ChannelB_Flit (AddrWidth p) (SizeWidth p) (SourceWidth p)

type ChannelC (p :: TLParamsKind) =
  ChannelC_Flit (AddrWidth p) (LaneWidth p) (SizeWidth p) (SourceWidth p)

type ChannelD (p :: TLParamsKind) =
  ChannelD_Flit (LaneWidth p) (SizeWidth p) (SourceWidth p) (SinkWidth p)

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
