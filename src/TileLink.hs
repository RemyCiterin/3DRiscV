{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TileLink where

import Blarney hiding(Vercor)
import Blarney.SourceSink
import Blarney.Connectable

import Blarney.TaggedUnion
import Arbiter
import Ehr

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

getLaneMask :: forall p. (KnownTLParams p) => Bit (AddrWidth p) -> Bit (SizeWidth p) -> Bit (LaneWidth p)
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

data GetMaster iw (p :: TLParamsKind) =
  GetMaster
    { canGet :: Bit 1
    , get :: Bit iw -> Bit (AddrWidth p) -> Bit (SizeWidth p) -> Action ()
    , canGetAck :: Bit 1
    , getAck :: Action ()
    , active :: Bit 1
    , address :: Bit (AddrWidth p)
    , index :: Bit iw }

makeGetMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> TLSlave p
    -> Module (GetMaster iw p)
makeGetMaster source arbiter ram slave = do
  metaD <- makeMetaSourceD @p slave.channelD
  let channelD = metaD.source

  mask :: Reg (Bit (LaneWidth p)) <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false
  last :: Reg (Bit 1) <- makeReg false

  request :: Reg (Bit iw, Bit (AddrWidth p)) <- makeReg dontCare

  always do
    when (channelD.canPeek) do
      let msg = channelD.peek
      when (msg.opcode `is` #AccessAckData .&&. msg.source === source) do
        arbiter.request

      when (arbiter.grant) do
        channelD.consume

        ram.storeBE index.val mask.val msg.lane
        index <== index.val + 1
        last <== metaD.last

  let init opcode idx addr size = do
        slave.channelA.put
          ChannelA
            { opcode= opcode
            , lane= dontCare
            , mask= getLaneMask @p addr size
            , address= addr
            , size= size
            , source= source}
        mask <== getLaneMask @p addr size
        request <== (idx,addr)
        valid <== true
        index <== idx

  return GetMaster
    { get= \ idx addr size -> do
        init (tag #Get ()) idx addr size
    , getAck= do
        valid <== false
        last <== false
    , canGet= slave.channelA.canPut .&&. inv valid.val
    , canGetAck= last.val
    , active= valid.val
    , address= request.val.snd
    , index= request.val.fst}

data PutMaster iw (p :: TLParamsKind) =
  PutMaster
    { canPut :: Bit 1
    , put :: Bit iw -> Bit (AddrWidth p) -> Bit (SizeWidth p) -> Action ()
    , canPutAck :: Bit 1
    , putAck :: Action ()
    , active :: Bit 1
    , address :: Bit (AddrWidth p)
    , index :: Bit iw }

makePutMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> TLSlave p
    -> Module (PutMaster iw p)
makePutMaster source arbiter ram slave = do
  let laneSize = toInteger $ log2 $ valueOf @(LaneWidth p)
  metaD <- makeMetaSourceD @p slave.channelD
  let channelD = metaD.source

  buffer :: Ehr (Bit (8 * LaneWidth p)) <- makeEhr 2 dontCare
  updBuf :: Reg (Bit 1) <- makeDReg false

  message :: Reg (ChannelA p) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false

  size :: Ehr TLSize <- makeEhr 2 0
  index :: Ehr (Bit iw) <- makeEhr 2 dontCare

  request :: Reg (Bit iw, Bit (AddrWidth p)) <- makeReg dontCare

  always do
    when (size.read 1 =!= 0) do
      arbiter.request
    when (arbiter.grant) do
      ram.loadBE (index.read 1)
      updBuf <== true

    when (updBuf.val) do
      buffer.write 0 ram.outBE

    when (size.read 0 =!= 0 .&&. slave.channelA.canPut) do
      let msg = (message.val { lane= buffer.read 1 } :: ChannelA p)
      slave.channelA.put msg

      index.write 0 (index.read 0 + 1)
      size.write 0 (size.read 0 - fromInteger laneSize)

  let init idx addr logSize = do
        message <==
          ChannelA
            { opcode= tag #PutData ()
            , lane= dontCare
            , mask= getLaneMask @p addr logSize
            , address= addr
            , source= source
            , size= logSize}
        size.write 0 (1 .<<. logSize)
        request <== (idx,addr)
        index.write 0 idx
        valid <== true

  return PutMaster
    { put= init
    , putAck= do
        channelD.consume
        valid <== false
    , canPut= inv valid.val
    , canPutAck=
        channelD.canPeek .&&. channelD.peek.opcode `is` #AccessAck .&&.
        channelD.peek.source === source .&&. size.read 0 === 0
    , active= valid.val
    , address= request.val.snd
    , index= request.val.fst}


data AcquireMaster iw (p :: TLParamsKind) =
  AcquireMaster
    { canAcquire :: Bit 1
    , acquireBlock :: Grow -> Bit iw -> Bit (AddrWidth p) -> Action ()
    , acquirePerms :: Grow -> Bit (AddrWidth p) -> Action ()
    , acquireAck :: Action TLPerm
    , canAcquireAck :: Bit 1
    , active :: Bit 1
    , address :: Bit (AddrWidth p)
    , index :: Bit iw}

makeAcquireMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> Bit (SizeWidth p)
    -> ArbiterClient
    -> RAM (Bit iw) (Bit (8*(LaneWidth p)))
    -> TLSlave p
    -> Module (AcquireMaster iw p)
makeAcquireMaster source logSize arbiter ram slave = do
  metaD <- makeMetaSourceD @p slave.channelD
  let channelD = metaD.source

  index :: Reg (Bit iw) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false
  last :: Reg (Bit 1) <- makeReg false

  sink :: Reg (Bit (SinkWidth p)) <- makeReg dontCare

  perm :: Reg TLPerm <- makeReg dontCare

  request :: Reg (Bit iw, Bit (AddrWidth p)) <- makeReg dontCare

  always do
    when (channelD.canPeek) do
      let msg = channelD.peek
      when (msg.opcode `is` #GrantData .&&. msg.source === source) do
        arbiter.request

      when (arbiter.grant) do
        let p = untag #GrantData msg.opcode
        channelD.consume

        perm <==
          select [
            p `is` #N --> tag #Nothing (),
            p `is` #B --> tag #Branch (),
            p `is` #T --> tag #Trunk ()
          ]

        ram.store index.val msg.lane
        index <== index.val + 1
        last <== metaD.last
        sink <== msg.sink

      when (msg.opcode `is` #Grant .&&. msg.source === source) do
        let p = untag #Grant msg.opcode
        channelD.consume

        perm <==
          select [
            p `is` #N --> tag #Nothing (),
            p `is` #B --> tag #Branch (),
            p `is` #T --> tag #Trunk ()
          ]

        last <== metaD.last
        sink <== msg.sink

  let init g idx addr opcode = do
        slave.channelA.put
          ChannelA
            { opcode= opcode
            , lane= dontCare
            , mask= ones
            , address= addr
            , size= logSize
            , source= source}
        request <== (idx,addr)
        valid <== true
        index <== idx

  return AcquireMaster
    { acquireBlock= \ g idx addr -> do
        init g idx addr (tag #AcquireBlock g)
    , acquirePerms= \ g addr -> do
        init g dontCare addr (tag #AcquireBlock g)
    , acquireAck= do
        slave.channelE.put ChannelE{sink= sink.val}
        valid <== false
        last <== false
        return perm.val
    , canAcquire= slave.channelA.canPut .&&. inv valid.val
    , canAcquireAck= last.val
    , active= valid.val
    , address= request.val.snd
    , index= request.val.fst}

data MshrMaster token iw (p :: TLParamsKind) =
  MshrMaster
    { canAcquire :: Bit 1
    , acquireBlock :: Grow -> Bit iw -> Bit (AddrWidth p) -> Action token
    , acquirePerms :: Grow -> Bit (AddrWidth p) -> Action token
    , canAcquireAck :: Bit 1
    , acquireAck :: Action (token, TLPerm)
    , searchAddress :: Bit (AddrWidth p) -> Bit 1
    , searchIndex :: Bit iw -> Bit 1 }

makeMshrMaster ::
  forall iw p token logMshr.
    (KnownNat logMshr, KnownNat (2^logMshr), KnownNat iw, KnownTLParams p, token ~ Bit logMshr)
      => [Bit (SourceWidth p)]
      -> Bit (SizeWidth p)
      -> ArbiterClient
      -> RAM (Bit iw) (Bit (8*(LaneWidth p)))
      -> TLSlave p
      -> Module (MshrMaster token iw p)
makeMshrMaster sources logSize arbiter ram slave = do
  let mshr = 2 ^ valueOf @logMshr
  acquireM :: [AcquireMaster iw p] <-
    sequence [makeAcquireMaster (sources!i) logSize arbiter ram slave | i <- [0..mshr-1]]

  acquire_block_w :: Wire (Grow, Bit iw, Bit (AddrWidth p)) <- makeWire dontCare
  acquire_perms_w :: Wire (Grow, Bit (AddrWidth p)) <- makeWire dontCare
  free_w :: Wire () <- makeWire ()

  let searchAddress address =
        orList [acq.active .&&. acq.address === address | acq <- acquireM]

  let searchIndex index =
        orList [acq.active .&&. acq.index === index | acq <- acquireM]

  let newHot :: Bit (2^logMshr) =
        firstHot (fromBitList [acq.active | acq <- acquireM])
  let newToken :: Bit logMshr =
        select [unsafeAt i newHot --> fromInteger (toInteger i) | i <- [0..mshr-1]]

  let freeHot :: Bit (2^logMshr) =
        firstHot (fromBitList [acq.canAcquireAck | acq <- acquireM])
  let freeToken :: Bit logMshr =
        select [unsafeAt i freeHot --> fromInteger (toInteger i) | i <- [0..mshr-1]]

  perms_w :: Wire TLPerm <- makeWire dontCare

  always do
    when acquire_perms_w.active do
      let (grant, addr) = acquire_perms_w.val
      sequence_
        [when (unsafeAt i newHot) do
          (acquireM!i).acquirePerms grant addr
        | i <- [0..mshr-1]]

    when acquire_block_w.active do
      let (grant, index, addr) = acquire_block_w.val
      sequence_
        [when (unsafeAt i newHot) do
          (acquireM!i).acquireBlock grant index addr
        | i <- [0..mshr-1]]

    when free_w.active do
      sequence_
        [when (unsafeAt i freeHot) do
          perm <- (acquireM!i).acquireAck
          perms_w <== perm
        | i <- [0..mshr-1]]

  return MshrMaster
    { canAcquire= orList [acq.canAcquire | acq <- acquireM] .&&. slave.channelA.canPut
    , acquireBlock= \ grow idx addr -> do
        acquire_block_w <== (grow, idx, addr)
        return newToken
    , acquirePerms= \ grow addr -> do
        acquire_perms_w <== (grow, addr)
        return newToken
    , canAcquireAck= orList [acq.canAcquireAck | acq <- acquireM] .&&. slave.channelE.canPut
    , acquireAck= do
        free_w <== ()
        return (freeToken, perms_w.val)
    , searchAddress
    , searchIndex }


data ProbeFSM iw (p :: TLParamsKind) =
  ProbeFSM
    { canStart :: Bit 1
    , start :: Bit iw -> OpcodeB -> Bit (AddrWidth p) -> Bit (SourceWidth p) -> Action ()
    , canWrite :: Bit 1
    , write :: Action (Bit iw, Bit (LaneWidth p), Bit 1)
    , exclusive :: Bit 1
    , hasData :: Bit 1
    , canAck :: Bit 1
    , ack :: Action ()}
