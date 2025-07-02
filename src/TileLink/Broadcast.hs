module TileLink.Broadcast where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Queue
import Blarney.Ehr

import Blarney.ADT
import Blarney.Arbiter
import Blarney.Sharing

import TileLink.Utils
import TileLink.Types
import TileLink.Interconnect

import Blarney.Option

import Data.List as List
import Blarney.Core.RTL(Displayable)

import Data.Proxy

logprint :: FShow a => a -> Action ()
logprint = \ _ -> pure () -- display "Slave: "

-- Define a broadcast cache coherency controller. This controller is named
-- a broadcast controller because it just send a probe request to all the
-- caches agants it known each times it receive an acquire requets
-- This controller supports:
--    - Acquire and Release requests of size `logSize`, knowning that
--      `logSize` must be bigger than `Log2 (LaneWidth p)`
--    - PutData and Get requests of size less than `Log2 (LaneWidth p)`
--      (one burst requests)

data BroadcastConfig p =
  BroadcastConfig
    { sources :: [Bit (SourceWidth p)]
    , logSize :: Int
    , baseSink :: Bit (SinkWidth p) }

makeBroadcast :: forall p p'.
  ( p' ~ TLParams (AddrWidth p) (LaneWidth p) (SizeWidth p) (SinkWidth p) 0
  , KnownTLParams p )
    => BroadcastConfig p
    -> Module (TLSlave p, TLMaster p')
makeBroadcast config
  | config.logSize < log2 (valueOf @(LaneWidth p)) = error "unsuported block size"
  | otherwise = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneLogSize :: TLSize = constant $ toInteger $ log2 $ valueOf @(LaneWidth p)

  let numSources = length config.sources
  let logSize = constant $ toInteger config.logSize

  slaveA <- makeQueue
  slaveD <- makeQueue

  [slaveA1, slaveA2] <- makeSharedSinkA @p' 2 (toSink slaveA)

  let slave :: TLSlave p' =
        TLSlave
          { channelA= toSink slaveA1
          , channelB= nullSource
          , channelC= nullSink
          , channelD= toSource slaveD
          , channelE= nullSink }

  queueA :: Queue (ChannelA p) <- makeQueue
  queueB :: Queue (ChannelB p) <- makeQueue
  queueC :: Queue (ChannelC p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue
  queueE :: Queue (ChannelE p) <- makeQueue

  [channelD1, channelD2] <- makeSharedSinkD @p 2 (toSink queueD)

  metaC :: MetaSource (ChannelC p) <- makeMetaSourceC @p (toSource queueC)
  let channelC = metaC.source

  acquire :: AcquireFSM p <-
    makeAcquireFSM
      (config.baseSink+1)
      config.logSize
      config.sources
      (toSource queueA)
      (toSink queueB)
      metaC
      channelD1
      (toSource queueE)
      slave

  release <-
    makeReleaseFSM @p config.baseSink logSize metaC channelD2

  waitAck :: Reg (Bit 1) <- makeReg false

  always do
    if waitAck.val then do

      let sresp = slaveD.first
      when (slaveD.canDeq .&&. sresp.opcode `isTagged` #AccessAck .&&. sresp.source === config.baseSink) do
        waitAck <== false
        slaveD.deq

    else do
      -- wait for all the releases to finish before an acquire: ensure data is not corrupted
      when (queueA.canDeq .&&. queueA.first.opcode `isTagged` #AcquireBlock .&&. inv acquire.active) do
        dynamicAssert (queueA.first.size === logSize) "wrong size"
        acquire.start

      -- wait for all the releases to finish before an acquire: ensure data is not corrupted
      when (queueA.canDeq .&&. queueA.first.opcode `isTagged` #AcquirePerms .&&. inv acquire.active) do
        dynamicAssert (queueA.first.size === logSize) "wrong size"
        acquire.start

      -- write one value at a time (to optimize)
      when (release.canPeek .&&. slaveA2.canPut) do
        let (addr, lane, mask, last) = release.peek
        waitAck <== last
        release.consume

        slaveA2.put
          ChannelA
            { opcode= item #PutData
            , source= config.baseSink
            , address= addr
            , size= logSize
            , mask
            , lane }


  return
    ( TLSlave
        { channelA= toSink queueA
        , channelB= toSource queueB
        , channelC= toSink queueC
        , channelD= toSource queueD
        , channelE= toSink queueE }
    , TLMaster
        { channelA= toSource slaveA
        , channelB= nullSink
        , channelC= nullSource
        , channelD= toSink slaveD
        , channelE= nullSource } )

data GrantFSM (iw :: Nat) p =
  GrantFSM
    { start :: Bit iw -> Action ()
    , write :: Bit iw -> Bit (8 * LaneWidth p) -> Action ()
    , read :: Source (Bit (8 * LaneWidth p)) }

makeGrantFSM :: forall iw p. (KnownTLParams p, KnownNat iw) => Module (GrantFSM iw p)
makeGrantFSM = do
  buffer :: RAM (Bit iw) (Bit (8 * LaneWidth p)) <- makeDualRAMForward
  epochBuf :: RAM (Bit iw) (Bit 1) <- makeDualRAMForward -- initialised with zeros
  epoch :: Reg (Bit 1) <- makeReg 0

  index :: Reg (Bit iw) <- makeReg 0

  doRead :: Wire (Bit 1) <- makeWire false

  always do
    let newIdx = doRead.val ? (index.val+1, index.val)
    when doRead.val (index <== index.val + 1)
    epochBuf.load newIdx
    buffer.load newIdx

  return
    GrantFSM
      { start= \ idx -> do
          epoch <== inv epoch.val
          index <== idx
      , write= \ idx lane -> do
          epochBuf.store idx epoch.val
          buffer.store idx lane
      , read=
          Source
            { peek= buffer.out
            , canPeek= epochBuf.out === epoch.val
            , consume= doRead <== true }}

data AcquireFSM p =
  AcquireFSM
    { address :: Bit (AddrWidth p)
    , active :: Bit 1
    , start :: Action () }

makeAcquireFSM :: forall p.
  KnownTLParams p
    => Bit (SinkWidth p)
    -> Int
    -> [Bit (SourceWidth p)]
    -> Source (ChannelA p)
    -> Sink (ChannelB p)
    -> MetaSource (ChannelC p)
    -> Sink (ChannelD p)
    -> Source (ChannelE p)
    -> TLSlave (TLParams (AddrWidth p) (LaneWidth p) (SizeWidth p) (SinkWidth p) 0)
    -> Module (AcquireFSM p)
makeAcquireFSM sink logSize sources channelA channelB metaC channelD channelE slave = do
  let blockWidth = logSize - log2 (valueOf @(LaneWidth p))
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  let laneLogSize :: TLSize = constant $ toInteger $ log2 $ valueOf @(LaneWidth p)
  let logSize' = constant $ toInteger logSize


  liftNat blockWidth $ \ (_ :: Proxy iw) -> do
    let getIndex :: Bit (AddrWidth p) -> Bit iw = \ addr ->
          unsafeSlice (logSize-1, log2 (valueOf @(LaneWidth p))) addr

    buffer <- makeGrantFSM @iw @p

    probe :: ProbeFSM iw p <- makeProbeFSM sink logSize' channelB metaC sources

    msg :: Reg (ChannelA p) <- makeReg dontCare
    valid :: Reg (Bit 1) <- makeReg false

    size :: Reg TLSize <- makeReg 0

    index1 :: Reg (Bit iw) <- makeReg dontCare

    waitAck :: Reg (Bit 1) <- makeReg false

    grant :: Reg (Bit 1) <- makeReg false

    always do
      -- write request
      when (probe.write.canPeek .&&. slave.channelA.canPut) do
        dynamicAssert (inv waitAck.val) "receive two ProbeAckData"
        let (index, lane, mask, last) = probe.write.peek
        probe.write.consume

        when (inv (msg.val.opcode `isTagged` #AcquirePerms)) do
          buffer.write index lane

        slave.channelA.put
          ChannelA
            { opcode= item #PutData
            , address= msg.val.address
            , size= logSize'
            , source= sink
            , lane
            , mask }

        when last do
          waitAck <== true

      -- Read response
      let sresp = slave.channelD.peek
      when (slave.channelD.canPeek .&&. sresp.source === sink .&&. sresp.opcode `isTagged` #AccessAckData) do
        buffer.write index1.val sresp.lane
        slave.channelD.consume

        index1 <== index1.val + 1

      -- Write response corresponding to a probe
      when (slave.channelD.canPeek .&&. sresp.source === sink .&&. sresp.opcode `isTagged` #AccessAck) do
        slave.channelD.consume
        waitAck <== false

      when (slave.channelA.canPut .&&. probe.stop.canPeek) do
        probe.stop.consume
        grant <== true

        when (inv probe.hasData) do
          slave.channelA.put
            ChannelA
              { mask= ones
              , address= msg.val.address
              , opcode= item #Get
              , lane= dontCare
              , size= logSize'
              , source= sink }

      if (msg.val.opcode `isTagged` #PutData) then do
        let trigger =
              channelA.canPeek .&&. channelD.canPut
              .&&. grant.val .&&. size.val =!= 0
              .&&. slave.channelA.canPut
              .&&. inv waitAck.val
        when trigger do
          size <== size.val .>. laneSize ? (size.val - laneSize, 0)

          channelD.put
            ChannelD
              { opcode= item #AccessAck
              , source= msg.val.source
              , size= msg.val.size
              , lane= dontCare
              , sink }

          slave.channelA.put
            ChannelA
              { opcode= item #PutData
              , address= msg.val.address
              , lane= channelA.peek.lane
              , mask= channelA.peek.mask
              , size= msg.val.size
              , source= sink }
          channelA.consume

          when (size.val .<=. laneSize) do
            valid <== false
            grant <== false
      else do
        when (channelD.canPut .&&. grant.val .&&. buffer.read.canPeek .&&. size.val =!= 0) do
          when (inv (msg.val.opcode `isTagged` #AcquirePerms)) do
            buffer.read.consume

          let cap = probe.exclusive ? (item #T, item #B)
          let opcode =
                select
                  [ msg.val.opcode `isTagged` #AcquirePerms --> tag #Grant cap
                  , msg.val.opcode `isTagged` #AcquireBlock --> tag #GrantData cap
                  , msg.val.opcode `isTagged` #Get --> item #AccessAckData ]

          size <== (size.val .>. laneSize .&&. hasDataD opcode) ? (size.val - laneSize, 0)

          channelD.put
            ChannelD
              { opcode
              , lane= buffer.read.peek
              , source= msg.val.source
              , size= msg.val.size
              , sink }

          when (msg.val.opcode `isTagged` #Get .&&. size.val .<=. laneSize) do
            valid <== false
            grant <== false

      when (channelE.canPeek .&&. size.val === 0 .&&. channelE.peek.sink === sink) do
        dynamicAssert grant.val "receive GrantAck without being in grant state"
        logprint channelE.peek
        channelE.consume
        valid <== false
        grant <== false

    return
      AcquireFSM
        { active= valid.val .||. waitAck.val
        , address= msg.val.address
        , start= do
            let acquire = channelA.peek
            dynamicAssert (inv valid.val) "invalid state"
            dynamicAssert (inv waitAck.val) "invalid state"
            dynamicAssert (index1.val === 0) "invalid state"

            when (acquire.opcode `isTagged` #AcquireBlock) do
              dynamicAssert (acquire.address .&. ((1 .<<. logSize') - 1) === 0) "unaligned access"
            when (acquire.opcode `isTagged` #AcquirePerms) do
              dynamicAssert (acquire.address .&. ((1 .<<. logSize') - 1) === 0) "unaligned access"

            let owners = [src =!= acquire.source | src <- sources]
            let perm = (decodeGrow (untag #AcquireBlock acquire.opcode)).snd
            let opcode =
                  channelA.peek.opcode `isTagged` #AcquirePerms ?
                  ( perm === trunk ? (tag #ProbePerms (item #N), tag #ProbePerms (item #B))
                  , perm === trunk ? (tag #ProbeBlock (item #N), tag #ProbeBlock (item #B)))
            probe.start.put (opcode, 0, acquire.address, owners)
            size <== 1 .<<. acquire.size

            when (inv (acquire.opcode `isTagged` #AcquirePerms)) do
              buffer.start (getIndex acquire.address)
            when (inv (acquire.opcode `isTagged` #PutData)) do
              channelA.consume
            msg <== acquire
            valid <== true
            logprint acquire }

makeReleaseFSM :: forall p.
  KnownTLParams p
    => Bit (SinkWidth p)
    -> Bit (SizeWidth p)
    -> MetaSource (ChannelC p)
    -> Sink (ChannelD p)
    -> Module (Source (Bit (AddrWidth p), Bit (8 * LaneWidth p), Bit (LaneWidth p), Bit 1))
makeReleaseFSM sink logSize metaC channelD = do
  let channelC = metaC.source
  let msgC = channelC.peek

  let address = zeroExtendCast metaC.offset + msgC.address

  let canPeek =
        channelC.canPeek
        .&&. channelD.canPut
        .&&. msgC.opcode `isTagged` #Release

  always do
    when canPeek do
      logprint msgC
      dynamicAssert (msgC.size === logSize) "wrong size"
      channelC.consume
      channelD.put
        ChannelD
          { opcode= item #ReleaseAck
          , lane= dontCare
          , size= logSize
          , source= msgC.source
          , sink }


  let canPeekData =
        channelC.canPeek
        .&&. msgC.opcode `isTagged` #ReleaseData
        .&&. (channelD.canPut .||. inv metaC.last)

  return
    Source
      { canPeek= canPeekData
      , peek= (address, msgC.lane, ones, metaC.last)
      , consume= do
          logprint msgC
          dynamicAssert (msgC.size === logSize) "wrong size"
          channelC.consume
          when (metaC.last) do
            channelD.put
              ChannelD
                { opcode= item #ReleaseAck
                , lane= dontCare
                , size= logSize
                , source= msgC.source
                , sink}}

data ProbeFSM iw p =
  ProbeFSM
    { start :: Sink (OpcodeB, Bit iw, Bit (AddrWidth p), [Bit 1])
    , write :: Source (Bit iw, Bit (8 * LaneWidth p), Bit (LaneWidth p), Bit 1)
    , stop :: Source ()
    , exclusive :: Bit 1
    , hasData :: Bit 1 }

makeProbeFSM :: forall iw p.
  (KnownNat iw, KnownTLParams p)
    => Bit (SinkWidth p)
    -> Bit (SizeWidth p)
    -> Sink (ChannelB p)
    -> MetaSource (ChannelC p)
    -> [Bit (SourceWidth p)]
    -> Module (ProbeFSM iw p)
makeProbeFSM sink logSize channelB metaC sources = do
  let channelC = metaC.source
  let msgC = channelC.peek

  let numSources = length sources

  needProbe :: [Reg (Bit 1)] <- replicateM numSources (makeReg false)
  probeAck :: [Reg (Bit 1)] <- replicateM numSources (makeReg false)

  let sourceOH =
        let blocked = List.inits [inv need.val | need <- needProbe] in
        [x.val .&&. andList y | (x,y) <- zip needProbe blocked]

  let source =
        select [oh --> s | (s,oh) <- zip sources sourceOH]

  let canGrant :: Bit 1 = andList [ack.val | ack <- probeAck]

  valid :: Reg (Bit 1) <- makeReg false
  opcode :: Reg OpcodeB <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  address :: Reg (Bit (AddrWidth p)) <- makeReg dontCare
  exclusive :: Reg (Bit 1) <- makeReg true
  hasData :: Reg (Bit 1) <- makeReg false

  always do
    when (valid.val .&&. orList [need.val | need <- needProbe] .&&. channelB.canPut) do
      channelB.put
        ChannelB
          { opcode= tag #ProbeBlock (item #N)
          , address= address.val
          , size= logSize
          , source }
      sequence_ [when oh (need <== false) | (oh,need) <- zip sourceOH needProbe]

    let trigger =
          valid.val
          .&&. channelC.canPeek
          .&&. msgC.address === address.val
          .&&. msgC.opcode `isTagged` #ProbeAck

    when (trigger) do
      let reduce = untag #ProbeAck msgC.opcode
      channelC.consume
      logprint msgC

      sequence_
        [ when (src === msgC.source) (ack <== true)
          | (ack, src) <- zip probeAck sources]
      when ((decodeReduce reduce).snd =!= nothing) do
        exclusive <== false

  return
    ProbeFSM
      { start=
          Sink
            { canPut= inv valid.val
            , put= \ (op, idx, addr, oh) -> do
                sequence_ [need <== x | (need, x) <- zip needProbe oh ]
                sequence_ [ack <== inv x | (ack, x) <- zip probeAck oh ]
                exclusive <== true
                hasData <== false
                address <== addr
                valid <== true
                opcode <== op
                index <== idx }
      , write=
          Source
            { canPeek=
                valid.val
                .&&. channelC.canPeek
                .&&. msgC.address === address.val
                .&&. msgC.opcode `isTagged` #ProbeAckData
            , peek= (index.val, msgC.lane, ones, metaC.last)
            , consume= do
                dynamicAssert (inv hasData.val) "receive two ProbeAckData"
                let reduce = untag #ProbeAckData msgC.opcode
                index <== index.val + 1
                channelC.consume
                logprint msgC

                when (metaC.last) do
                  sequence_
                    [ when (src === msgC.source) (ack <== true)
                      | (ack, src) <- zip probeAck sources]
                  when ((decodeReduce reduce).snd =!= nothing) do
                    exclusive <== false
                  hasData <== true }
      , stop=
          Source
            { peek= ()
            , canPeek= canGrant .&&. valid.val
            , consume= valid <== false }
      , exclusive= exclusive.val
      , hasData= hasData.val }
