{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module TileLink where

import Blarney hiding(Vercor)
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.TaggedUnion
import Blarney.Arbiter

import Data.Proxy

import TileLink.Utils
import TileLink.Types

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
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
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

      let newSize = size.read 0 .>=. laneSize ? (size.read 0 - laneSize,0)
      index.write 0 (index.read 0 + 1)
      size.write 0 newSize

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
      Nothing -> makeDualRAMBE
      Just name -> makeDualRAMInitBE name

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
        (channelA.peek.opcode `is` #PutData .||. channelA.peek.opcode `is` #Get)
        "makeTLRAM only allow PutData and Get requests"
      let isPut = channelA.peek.opcode `is` #PutData

      let addr = (channelA.peek.address - config.lowerBound) .>>. laneLogSize
      let sz = size.val === 0 ? (1 .<<. channelA.peek.size, size.val)
      let idx = size.val === 0 ? (truncate addr, index.val)
      let msb :: Bit (AddrWidth p - iw) = truncateLSBCast addr

      queue.enq
        ChannelD
          { opcode= isPut ? (tag #AccessAck (), tag #AccessAckData ())
          , source= channelA.peek.source
          , size= channelA.peek.size
          , lane= dontCare
          , sink= config.sink }

      if isPut then do
        when (msb === 0) do
          ram.storeBE idx channelA.peek.mask channelA.peek.lane
      else do
        ram.loadBE idx

      size <== sz .>. laneSize ? (sz - laneSize, 0)
      index <== idx + 1

      when (inv isPut .||. sz .<=. laneSize) do
        channelA.consume

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


-- data BurstFSM iw (p :: TLParamsKind) =
--   BurstFSM
--     { canStart :: Bit 1
--     , start :: OpcodeC -> Bit iw -> Bit (AddrWidth p) -> Action ()
--     , canStop :: Bit 1
--     , stop :: Action () }
--
-- makeBurstFSM :: forall iw p.
--   (KnownNat iw, KnownTLParams p)
--     => Bit (SourceWidth p)
--     -> TLSlave p
--     -> ArbiterClient
--     -> RAMBE iw (LaneWidth p)
--     -> Module (BurstFSM iw p)
-- makeBurstFSM slave arbiter ram = do
--   msg :: Reg (ChannelC p) <- makeReg dontCare
--   let opcode :: OpcodeC = msg.val.opcode
--
--   metaD <- makeMetaSourceD slave.channelD
--   let channelD = metaD.source
--
--   let isRelease =
--         opcode `is` #ReleaseData .||. opcode `is` #Release
--
--   let releaseAck =
--         channelD.canPeek
--         .&&. channelD.peek.opcode `is` #ReleaseAck
--         .&&. channelD.peek.source === source
--
--   -- queue between stage 1 and 2
--   queue :: Queue () <- makePipelineQueue 1
--
--   always do
--     when (queue.notFull .&&. opcode `is` #ReleaseData) do
--       arbiter.request
--
--     when (queue.notFull .&&. opcode `is` #ProbeAckData) do
--       arbiter.request

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
