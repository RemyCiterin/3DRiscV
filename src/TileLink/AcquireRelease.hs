module TileLink.AcquireRelease where

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

data AcquireMaster iw (p :: TLParamsKind) =
  AcquireMaster
    { canAcquire :: Bit 1
    , acquireBlock :: Grow -> Bit iw -> Bit (AddrWidth p) -> Action ()
    , acquirePerms :: Grow -> Bit (AddrWidth p) -> Action ()
    , acquireAck :: Action TLPerm
    , canAcquireAck :: Bit 1
    , active :: Bit 1
    , address :: Bit (AddrWidth p)
    , index :: Bit iw }

makeAcquireMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> Bit (SizeWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
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
      when (msg.opcode `isTagged` #GrantData .&&. msg.source === source) do
        arbiter.request

      when (arbiter.grant) do
        let p = untag #GrantData msg.opcode
        channelD.consume

        perm <==
          select [
            p `isTagged` #N --> nothing,
            p `isTagged` #B --> branch,
            p `isTagged` #T --> trunk
          ]

        ram.storeBE index.val (getLaneMask @p request.val.snd logSize) msg.lane
        index <== index.val + 1
        last <== metaD.last
        sink <== msg.sink

      when (msg.opcode `isTagged` #Grant .&&. msg.source === source) do
        let p = untag #Grant msg.opcode
        channelD.consume

        perm <==
          select [
            p `isTagged` #N --> nothing,
            p `isTagged` #B --> branch,
            p `isTagged` #T --> trunk
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
    , index= request.val.fst }

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
      -> RAMBE iw (LaneWidth p)
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
        opcode `isTagged` #ReleaseData .||. opcode `isTagged` #Release

  let releaseAck =
        slave.channelD.canPeek
        .&&. slave.channelD.peek.opcode `isTagged` #ReleaseAck
        .&&. slave.channelD.peek.source === source

  -- queue between stage 1 and 2
  queue :: Queue () <- makePipelineQueue 1

  always do
    when (size.val =!= 0 .&&. queue.notFull .&&. hasDataC opcode) do
      if hasDataC opcode then do
        arbiter.request
      else do
        slave.channelC.put msg.val
        size <== 0

    when (arbiter.grant) do
      size <== size.val .>=. laneSize ? (size.val - laneSize, 0)
      index <== index.val + 1
      ram.loadBE index.val
      queue.enq ()

    when (queue.canDeq .&&. slave.channelC.canPut) do
      slave.channelC.put (msg.val{lane= ram.outBE} :: ChannelC p)
      queue.deq

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
              , lane= dontCare }
          size <== 1 .<<. laneSize
          valid <== true
          index <== idx
      , canStop=
          valid.val .&&. size.val === 0
          .&&. (releaseAck .||. inv isRelease)
      , stop= do
          when isRelease do
            slave.channelD.consume
          valid <== false }

data ProbeMaster iw p =
  ProbeMaster
    { start :: Source (Bit (AddrWidth p), TLPerm)
    , evict :: Sink (Reduce, Option (Bit iw))
    , ack :: Source () }

data ReleaseMaster iw p =
  ReleaseMaster
    { start :: Sink (Reduce, Bit (AddrWidth p), Option (Bit iw))
    , ack :: Source () }

makeReleaseMaster :: forall iw p.
  (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> Bit (SizeWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> TLSlave p
    -> Module (ReleaseMaster iw p, ProbeMaster iw p)
makeReleaseMaster source logSize arbiter ram slave = do
  burstM <- makeBurstFSM @iw @p source slave arbiter ram

  state :: Reg (Bit 3) <- makeReg 0

  let message = slave.channelB.peek
  let canProbe =
        slave.channelB.canPeek .&&. message.source === source .&&. state.val === 0

  let opcode :: OpcodeB = message.opcode
  let cap :: Cap =
        select
          [ opcode `isTagged` #ProbePerms --> untag #ProbePerms opcode
          , opcode `isTagged` #ProbeBlock --> untag #ProbeBlock opcode ]

  let releaseM =
        ReleaseMaster
          { start=
              Sink
                { canPut= state.val === 0 .&&. burstM.canStart
                , put= \ (reduce, addr, index) -> do
                    state <== 3
                    let op =
                          index.valid ? (tag #ReleaseData reduce, tag #Release reduce)
                    burstM.start op index.val addr logSize }
          , ack=
              Source
                { peek= ()
                , canPeek= state.val === 3 .&&. burstM.canStop
                , consume= do
                    burstM.stop
                    state <== 0 }}

  let probeM =
        ProbeMaster
          { start=
              Source
                { canPeek= canProbe
                , peek= (message.address, capTo cap)
                , consume= state <== 1 }
          , evict=
              Sink
                { canPut= state.val === 1 .&&. burstM.canStart
                , put= \ (reduce, index) -> do
                    let op =
                          (index.valid .&&. opcode `isTagged` #ProbeBlock) ?
                            (tag #ProbeAckData reduce, tag #ProbeAck reduce)
                    burstM.start op index.val message.address logSize
                    state <== 2 }
          , ack=
              Source
                { peek= ()
                , canPeek= state.val === 2 .&&. burstM.canStop
                , consume= do
                    state <== 0
                    burstM.stop
                    slave.channelB.consume }}
  return (releaseM, probeM)
