module TileLink.Types where

import Blarney hiding(Vercor)
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.ADT
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

data TLPerm = TLPerm (Bit 2) deriving(Generic, Bits, Cmp)

-- nothing is encodded to zero, such that rams are initialised with it
nothing, trunk, dirty, branch :: TLPerm
nothing = TLPerm 0
branch = TLPerm 1
trunk = TLPerm 2
dirty = TLPerm 3

instance FShow TLPerm where
  fshow perm =
    formatCond (perm === nothing) (fshow "nothing") <>
    formatCond (perm === branch) (fshow "branch") <>
    formatCond (perm === trunk) (fshow "trunk") <>
    formatCond (perm === dirty) (fshow "dirty")

type Grow =
  TaggedUnion [
    "NtoB" ::: (),
    "NtoT" ::: (),
    "BtoT" ::: ()
  ]

n2b, n2t, b2t :: Grow
n2b = tag #NtoB ()
n2t = tag #NtoT ()
b2t = tag #BtoT ()

decodeGrow :: Grow -> (TLPerm, TLPerm)
decodeGrow grow =
  select
    [ grow === n2b --> (nothing, branch)
    , grow === n2t --> (nothing, trunk)
    , grow === b2t --> (branch, trunk) ]

formatGrow :: Grow -> Format
formatGrow perm =
  formatCond (perm `isTagged` #NtoB) (fshow "NtoB") <>
  formatCond (perm `isTagged` #NtoT) (fshow "NtoT") <>
  formatCond (perm `isTagged` #BtoT) (fshow "BtoT")

type Reduce =
  TaggedUnion [
    "TtoB" ::: (),
    "TtoN" ::: (),
    "BtoB" ::: (),
    "TtoT" ::: (),
    "BtoN" ::: (),
    "NtoN" ::: ()
  ]

t2t, t2b, t2n, b2b, b2n, n2n :: Reduce
t2t = tag #TtoT ()
t2b = tag #TtoB ()
t2n = tag #TtoN ()
b2b = tag #BtoB ()
b2n = tag #BtoN ()
n2n = tag #NtoN ()

decodeReduce :: Reduce -> (TLPerm, TLPerm)
decodeReduce reduce =
  select
    [ reduce === t2t --> (trunk, trunk)
    , reduce === t2b --> (trunk, branch)
    , reduce === t2n --> (trunk, nothing)
    , reduce === b2b --> (branch, branch)
    , reduce === b2n --> (branch, nothing)
    , reduce === n2n --> (nothing, nothing) ]

formatReduce :: Reduce -> Format
formatReduce perm =
  formatCond (perm `isTagged` #TtoB) (fshow "TtoB") <>
  formatCond (perm `isTagged` #TtoN) (fshow "TtoN") <>
  formatCond (perm `isTagged` #BtoB) (fshow "BtoB") <>
  formatCond (perm `isTagged` #TtoT) (fshow "TtoT") <>
  formatCond (perm `isTagged` #BtoN) (fshow "BtoN") <>
  formatCond (perm `isTagged` #NtoN) (fshow "NtoN")

type Cap =
  TaggedUnion [
    "T" ::: (),
    "B" ::: (),
    "N" ::: ()
  ]

decodeCap :: Cap -> TLPerm
decodeCap cap =
  select
    [ cap `isTagged` #T --> trunk
    , cap `isTagged` #B --> branch
    , cap `isTagged` #N --> nothing ]

formatCap :: Cap -> Format
formatCap perm =
  formatCond (perm `isTagged` #T) (fshow "T") <>
  formatCond (perm `isTagged` #B) (fshow "B") <>
  formatCond (perm `isTagged` #N) (fshow "N")

type OpcodeA =
  TaggedUnion [
    "AcquirePerms" ::: Grow,
    "AcquireBlock" ::: Grow,
    "PutData" ::: (),
    "Get" ::: ()
  ]

formatOpcodeA :: OpcodeA -> Format
formatOpcodeA opcode =
  formatCond (opcode `isTagged` #Get) (fshow "Get") <>
  formatCond (opcode `isTagged` #PutData) (fshow "PutData") <>
  formatCond
    (opcode `isTagged` #AcquireBlock)
    (fshow "AcquireBlock<" <> formatGrow (untag #AcquireBlock opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #AcquirePerms)
    (fshow "AcquirePerms<" <> formatGrow (untag #AcquirePerms opcode) <> fshow ">")

type OpcodeB =
  TaggedUnion [
    "ProbePerms" ::: Cap,
    "ProbeBlock" ::: Cap
  ]


formatOpcodeB :: OpcodeB -> Format
formatOpcodeB opcode =
  formatCond
    (opcode `isTagged` #ProbePerms)
    (fshow "ProbePerms<" <> formatCap (untag #ProbePerms opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #ProbeBlock)
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
    (opcode `isTagged` #ProbeAck)
    (fshow "ProbeAck<" <> formatReduce (untag #ProbeAck opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #ProbeAckData)
    (fshow "ProbeAckData<" <> formatReduce (untag #ProbeAckData opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #Release)
    (fshow "Release<" <> formatReduce (untag #Release opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #ReleaseData)
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
  formatCond (opcode `isTagged` #ReleaseAck) (fshow "ReleaseAck") <>
  formatCond (opcode `isTagged` #AccessAck) (fshow "AccessAck") <>
  formatCond (opcode `isTagged` #AccessAckData) (fshow "AccessAckData") <>
  formatCond
    (opcode `isTagged` #Grant)
    (fshow "Grant<" <> formatCap (untag #Grant opcode) <> fshow ">") <>
  formatCond
    (opcode `isTagged` #GrantData)
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
