{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TileLink where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.TaggedUnion
import Blarney.Arbiter


import TileLink.Utils
import TileLink.Types


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


data BurstFSM iw (p :: TLParamsKind) =
  BurstFSM
    { canStart :: Bit 1
    , start :: OpcodeC -> Bit iw -> Bit (AddrWidth p) -> Bit (SizeWidth p) -> Action ()
    , canStop :: Bit 1
    , stop :: Action () }

makeBurstFSM :: forall iw p.
  (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> TLSlave p
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> Module (BurstFSM iw p)
makeBurstFSM source slave arbiter ram = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  msg :: Reg (ChannelC p) <- makeReg dontCare
  let opcode :: OpcodeC = msg.val.opcode

  size :: Reg TLSize <- makeReg 0
  index :: Reg (Bit iw) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false

  let isRelease =
        opcode `is` #ReleaseData .||. opcode `is` #Release

  let releaseAck =
        slave.channelD.canPeek
        .&&. slave.channelD.peek.opcode `is` #ReleaseAck
        .&&. slave.channelD.peek.source === source

  -- queue between stage 1 and 2
  queue :: Queue () <- makePipelineQueue 1

  always do
    when (size.val =!= 0 .&&. queue.notFull .&&. hasDataC opcode) do
      size <== size.val .>=. laneSize ? (size.val - laneSize, 0)
      index <== index.val + 1
      ram.loadBE index.val
      arbiter.request
      queue.enq ()

    when (queue.canDeq .&&. hasDataC opcode .&&. slave.channelC.canPut) do
      slave.channelC.put (msg.val{lane= ram.outBE} :: ChannelC p)
      queue.deq

    when (slave.channelC.canPut .&&. size.val =!= 0 .&&. inv (hasDataC opcode)) do
      slave.channelC.put msg.val
      size <== 0

  return
    BurstFSM
      { canStart= inv valid.val .&&. inv queue.canDeq
      , start= \ op idx addr logSize -> do
          msg <==
            ChannelC
              { opcode= op
              , address= addr
              , size= logSize
              , source= source
              , lane= dontCare}
          size <== 1 .<<. laneSize
          valid <== true
          index <== idx
          return ()
      , canStop=
          valid.val .&&. size.val === 0
          .&&. (releaseAck .||. inv isRelease)
      , stop= do
          when (isRelease) do
            slave.channelD.consume
          valid <== false
          return ()}

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
