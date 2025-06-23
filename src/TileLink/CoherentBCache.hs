module TileLink.CoherentBCache
  ( BCacheRequest
  , BCacheCore(..)
  , makeBCacheCoreWith
  , makeBCacheCore
  ) where

import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Sharing
import Blarney.Arbiter
import Blarney.SourceSink
import Blarney.ADT

import TileLink.Types
import TileLink.Utils
import TileLink.AcquireRelease
import Utils

storeListRAM :: (Bits a, Bits b, KnownNat idx) => [RAM a b] -> Bit idx -> a -> b -> Action ()
storeListRAM rams index k v =
  sequence_ [when (constant i === index) do r.store k v | (r,i) <- zip rams [0..]]

loadListRAM :: (Bits a, Bits b, KnownNat idx) => [RAM a b] -> Bit idx -> a -> Action ()
loadListRAM rams index k =
  sequence_ [when (constant i === index) do r.load k | (r,i) <- zip rams [0..]]

outListRAM :: (Bits a, Bits b, KnownNat idx) => [RAM a b] -> Bit idx -> b
outListRAM rams index =
  select [constant i === index --> r.out | (r,i) <- zip rams [0..]]


type BCacheRequest atomic p =
  TaggedUnion
    [ "Load" ::: ()
    , "LoadR" ::: ()
    , "Atomic" ::: atomic
    , "StoreC" ::: (Bit (LaneWidth p), Bit (8 * LaneWidth p))
    , "Store" ::: (Bit (LaneWidth p), Bit (8 * LaneWidth p))]

needPermTrunk :: forall atomic p.
  (Bits atomic, KnownTLParams p) => BCacheRequest atomic p -> Bit 1
needPermTrunk req = inv (req `isTagged` #Load)

satisfyPerms :: forall atomic p.
  (Bits atomic, KnownTLParams p) => BCacheRequest atomic p -> TLPerm -> Bit 1
satisfyPerms req perm =
  perm =!= nothing .&&. inv (perm === branch .&&. needPermTrunk @atomic @p req)

-- Operations are performed in order here, no NACK at matching stage
data BCacheCore kw iw ow atomic p =
  BCacheCore
    { canLookup :: Bit 1
    , lookup :: Bit iw -> Bit ow -> BCacheRequest atomic p -> Action ()
    , canMatch :: Bit 1
    , match :: Bit kw -> Action ()
    , abort :: Action ()
    , loadResponse :: Source (Bit (8*LaneWidth p))
    , scResponse :: Source (Bit 1)
    , atomicResponse :: Source (Bit (8*LaneWidth p)) }

type BCacheState = Bit 3
st_idle, st_lookup, st_acquire, st_release, st_probe_lookup, st_probe_burst :: BCacheState
st_idle = 0
st_lookup= 1
st_acquire= 2
st_release= 3
st_probe_lookup= 4
st_probe_burst=5

encode :: forall p kw iw ow.
  (KnownNat kw
  , KnownNat iw
  , KnownNat ow
  , KnownTLParams p
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
  => Bit kw
  -> Bit iw
  -> Bit ow
  -> Bit (AddrWidth p)
encode key index offset =
  cast (key # index # offset # (0 :: Bit (Log2 (LaneWidth p))))

decode :: forall p kw iw ow.
  (KnownNat kw
  , KnownNat iw
  , KnownNat ow
  , KnownTLParams p
  , KnownNat (Log2 (LaneWidth p)))
  => Bit (AddrWidth p)
  -> (Bit kw, Bit iw, Bit ow)
decode addr =(key, index, offset)
  where
    key = truncateLSBCast addr
    index = unsafeSlice (startKey-1,startIndex) addr
    offset = unsafeSlice (startIndex-1,startOffset) addr
    startOffset = valueOf @(Log2 (LaneWidth p))
    startIndex = startOffset + valueOf @ow
    startKey = startIndex + valueOf @iw

makeBCacheCoreWith :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , KnownNat (w+(iw+ow))
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
    => Bit (SourceWidth p)
    -> TLSlave p
    -> (atomic -> Bit (8 * LaneWidth p) -> Bit (8 * LaneWidth p))
    -> Module (BCacheCore kw iw ow atomic p)
makeBCacheCoreWith source slave execAtomic = do
  putArbiter <- makeNullArbiter
  getArbiter <- makeNullArbiter

  reserved :: Reg (Bit 1) <- makeReg false

  dataQueue :: Queue (Option (atomic, Bit (w+(iw+ow)))) <- makePipelineQueue 1

  scResponseQ :: Queue (Bit 1) <- makeQueue

  state :: Ehr BCacheState <- makeEhr 2 st_idle
  nextState :: Reg BCacheState <- makeReg dontCare

  let ways = 2^(valueOf @w)
  permRam :: [RAM (Bit iw) TLPerm] <- replicateM ways makeDualRAMForward
  keyRam :: [RAM (Bit iw) (Bit kw)] <- replicateM ways makeDualRAMForward
  [dataRamA,dataRamB] <- makeDualRAMForwardBE >>= makeSharedRAMBE 2

  let logSize :: Bit (SizeWidth p) =
        constant $ toInteger $ valueOf @(AddrWidth p) - valueOf @kw - valueOf @iw

  acquireM <- makeAcquireMaster @(w+(iw+ow)) @p source logSize putArbiter dataRamB slave
  (releaseM, probeM) <- makeReleaseMaster @(w+(iw+ow)) @p source logSize getArbiter dataRamB slave

  randomWay :: Reg (Bit w) <- makeReg dontCare
  always do randomWay <== randomWay.val + 1

  request :: Reg (BCacheRequest atomic p) <- makeReg dontCare
  offset :: Reg (Bit ow) <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  key :: Reg (Bit kw) <- makeReg dontCare
  way :: Reg (Bit w) <- makeReg dontCare
  cap :: Reg TLPerm <- makeReg dontCare

  let execOp :: Bit w -> Action () = \ way -> do
        reserved <== request.val `isTagged` #LoadR
        when (request.val `isTagged` #Load .||. request.val `isTagged` #LoadR) do
          dataRamA.loadBE (way # index.val # offset.val)
          dataQueue.enq none
        when (request.val `isTagged` #Atomic) do
          dataQueue.enq (some (untag #Atomic request.val, way # index.val # offset.val))
          dataRamA.loadBE (way # index.val # offset.val)
        when (request.val `isTagged` #Store) do
          let (mask,lane) = untag #Store request.val
          dataRamA.storeBE (way # index.val # offset.val) mask lane
        when (request.val `isTagged` #StoreC) do
          dynamicAssert (scResponseQ.notFull) "enq into a full queue"
          scResponseQ.enq reserved.val
          when (reserved.val) do
            let (mask,lane) = untag #Store request.val
            dataRamA.storeBE (way # index.val # offset.val) mask lane

  let baseAddr :: Bit kw -> Bit (AddrWidth p) = \ key ->
        encode @p @kw @iw @ow key index.val 0

  always do
    when (state.read 0 === st_release .&&. releaseM.ack.canPeek .&&. acquireM.canAcquire) do
      acquireM.acquireBlock (tag #NtoT ()) (way.val # index.val # 0) (baseAddr key.val)
      --display "acquire block: 0x" (formatHex 0 (baseAddr key.val))
      state.write 0 st_acquire
      releaseM.ack.consume

    when (state.read 0 === st_acquire .&&. acquireM.canAcquireAck) do
      storeListRAM keyRam way.val index.val key.val

      execOp way.val
      state.write 0 st_idle
      perm <- acquireM.acquireAck
      storeListRAM permRam way.val index.val (dataRamA.storeActiveBE ? (dirty,perm))

  always do
    when ((state.read 1 === st_idle .||. state.read 1 === st_acquire) .&&. probeM.start.canPeek) do
      let (addr, perm) = probeM.start.peek
      let (k,idx,off) = decode @p addr
      state.write 1 st_probe_lookup
      nextState <== state.read 1

      key <== k
      index <== idx
      offset <== off
      sequence_ [do
          p.load idx
          t.load idx
          | (p,t) <- zip permRam keyRam]
      cap <== perm

    when (state.read 0 === st_probe_lookup .&&. probeM.evict.canPut) do
      state.write 0 st_probe_burst

      let hit =
            orList
              [key.val === t.out .&&. p.out .>. nothing
                | (t,p) <- zip keyRam permRam]

      let (way :: Bit w, perm) =
            selectDefault (0, nothing)
              [key.val === t.out .&&. p.out .>. nothing --> (constant i, p.out)
                | (t,p,i) <- zip3 keyRam permRam [0..]]

      when (perm === nothing) do
        probeM.evict.put (n2n, none)
      when (perm === branch) do
        if cap.val === nothing then do
          probeM.evict.put (b2n, none)
          storeListRAM permRam way index.val nothing
        else do
          probeM.evict.put (b2b, none)
      when (perm .>=. trunk) do
        when (cap.val === trunk) do
          probeM.evict.put (t2t, none)
        when (cap.val === branch) do
          probeM.evict.put (t2b, some $ way # index.val # 0)
          storeListRAM permRam way index.val branch
          reserved <== false
        when (cap.val === nothing) do
          probeM.evict.put (t2n, some $ way # index.val # 0)
          storeListRAM permRam way index.val nothing
          reserved <== false

    when (state.read 0 === st_probe_burst .&&. probeM.ack.canPeek) do
      state.write 0 nextState.val
      probeM.ack.consume

  return
    BCacheCore
      { canLookup= state.read 1 === st_idle .&&. inv probeM.start.canPeek
      , lookup= \ idx off req -> do
          dynamicAssert (state.read 1 === st_idle) "lookup with an unexpected state"
          request <== req
          offset <== off
          index <== idx

          state.write 1 st_lookup
          sequence_ [do
              p.load idx
              t.load idx
              | (p,t) <- zip permRam keyRam]
      , canMatch=
          inv (dataQueue.canDeq .&&. dataQueue.first.valid .&&. request.val `isTagged` #Store)
          .&&. inv (dataQueue.canDeq .&&. dataQueue.first.valid .&&. request.val `isTagged` #StoreC)
          .&&. state.read 0 === st_lookup
          .&&. scResponseQ.notFull
          .&&. dataQueue.notFull
          .&&. acquireM.canAcquire
          .&&. releaseM.start.canPut
      , match= \ msb -> do
          dynamicAssert (state.read 0 === st_lookup) "matching with an unexpected state"
          key <== msb

          let hit =
                orList
                  [msb === t.out .&&. satisfyPerms @atomic @p request.val p.out
                    | (t,p) <- zip keyRam permRam]

          let hitWay :: Bit w =
                selectDefault randomWay.val
                  [msb === t.out .&&. p.out .>. nothing --> constant i
                    | (t,p,i) <- zip3 keyRam permRam [0..]]

          if hit then do
            execOp hitWay
            state.write 0 st_idle
            when (dataRamA.storeActiveBE) do
              storeListRAM permRam hitWay index.val dirty
          else do
            way <== hitWay
            reserved <== false
            if outListRAM permRam hitWay =!= nothing .&&. outListRAM keyRam hitWay =!= msb then do
              let oldPerm = outListRAM permRam hitWay
              let oldKey = outListRAM keyRam hitWay

              -- writeback only if the choosen block is dirty
              when (oldPerm === dirty) do
                releaseM.start.put (t2n, baseAddr oldKey, some $ hitWay # index.val # 0)
              when (oldPerm === trunk) do
                releaseM.start.put (t2n, baseAddr oldKey, none)
              when (oldPerm === branch) do
                releaseM.start.put (b2n, baseAddr oldKey, none)

              -- invalidate the permission of the choosen block
              storeListRAM permRam hitWay index.val nothing
              state.write 0 st_release
            else do
              state.write 0 st_acquire
              acquireM.acquireBlock
                (needPermTrunk @atomic @p request.val ? (n2t, n2b))
                (hitWay # index.val # 0)
                (baseAddr msb)

          return ()
      , abort= state.write 0 st_idle
      , scResponse= toSource scResponseQ
      , loadResponse=
          Source
            { canPeek= dataQueue.canDeq .&&. inv dataQueue.first.valid
            , consume= dataQueue.deq
            , peek= dataRamA.outBE}
      , atomicResponse=
          Source
            { canPeek= dataQueue.canDeq .&&. dataQueue.first.valid
            , consume= do
                let (op,pos) = dataQueue.first.val
                dataRamA.storeBE pos ones (execAtomic op dataRamA.outBE)
                dataQueue.deq
            , peek= dataRamA.outBE}}

makeBCacheCore :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , KnownNat (w+(iw+ow))
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
    => Bit (SourceWidth p)
    -> (atomic -> Bit (8 * LaneWidth p) -> Bit (8 * LaneWidth p))
    -> Module (BCacheCore kw iw ow atomic p, TLMaster p)
makeBCacheCore source execAtomic = do
  queueA <- makeQueue
  queueB <- makeQueue
  queueC <- makeQueue
  queueD <- makeQueue
  queueE <- makeQueue

  let slave =
        TLSlave
          { channelA= toSink queueA
          , channelB= toSource queueB
          , channelC= toSink queueC
          , channelD= toSource queueD
          , channelE= toSink queueE }
  let master =
        TLMaster
          { channelA= toSource queueA
          , channelB= toSink queueB
          , channelC= toSource queueC
          , channelD= toSink queueD
          , channelE= toSource queueE }

  ifc <- makeBCacheCoreWith @w @kw @iw @ow @atomic @p source slave execAtomic
  return (ifc,master)
