module TileLink.CoherentBCache
  ( BCacheRequest
  , BCacheCore(..)
  , BCacheStats(..)
  , makeBCacheCoreWith
  , makeBCacheCore
  , testBCacheCore
  ) where

import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Sharing
import Blarney.Arbiter
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Utils
import Blarney.ADT

import TileLink.RAM
import TileLink.Types
import TileLink.Utils
import TileLink.AcquireRelease
import TileLink.Interconnect
import TileLink.Broadcast

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

formatBCacheRequest :: forall atomic p. (KnownTLParams p, FShow atomic, Bits atomic) =>
  BCacheRequest atomic p -> Format
formatBCacheRequest request =
  formatCond (request `isTagged` #Load) formatLoad
  <> formatCond (request `isTagged` #LoadR) formatLoadR
  <> formatCond (request `isTagged` #Atomic) formatAtomic
  <> formatCond (request `isTagged` #Store)  formatStore
  <> formatCond (request `isTagged` #StoreC) formatStoreC
    where
      formatLoad = fshow "#Load"
      formatLoadR = fshow "#LoadR"
      formatStore =
        let (mask, lane) = untag #Store request in
        fshow "#Store{mask: " <> fshow mask <> fshow ", data: " <> (formatHex 0 lane) <> fshow "}"
      formatStoreC =
        let (mask, lane) = untag #StoreC request in
        fshow "#StoreC{mask: " <> fshow mask <> fshow ", data: " <> (formatHex 0 lane) <> fshow "}"
      formatAtomic =
        fshow "#Atomic{" <> fshow (untag #Atomic request) <> fshow "}"

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
    , atomicResponse :: Source (Bit (8*LaneWidth p))
    , stats :: BCacheStats }

data BCacheStats =
  BCacheStats
    { numAcquire :: Bit 32
    , numRelease :: Bit 32
    , numProbe :: Bit 32
    , numHit :: Bit 32
    , numReq :: Bit 32 }

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

-- To have dead-lock free cache coherency, users of a cache must ensure
-- that they always have enough space for the answer of the request before
-- sending it, otherwise a probe request may be blocked, resulting
-- in a cycle of agents waiting for each other.
makeBCacheCoreWith :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , FShow atomic
  , KnownNat (w+(iw+ow))
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
    => Bit (SourceWidth p)
    -> TLSlave p
    -> (atomic -> Bit (8 * LaneWidth p) -> Bit (8 * LaneWidth p))
    -> Module (BCacheCore kw iw ow atomic p)
makeBCacheCoreWith source slave execAtomic = do
  -- statistics generation
  numAcquire :: Reg (Bit 32) <- makeReg 0
  numRelease :: Reg (Bit 32) <- makeReg 0
  numProbe :: Reg (Bit 32) <- makeReg 0
  numReq :: Reg (Bit 32) <- makeReg 0
  numHit :: Reg (Bit 32) <- makeReg 0

  putArbiter <- makeNullArbiter
  getArbiter <- makeNullArbiter

  reserved :: Reg (Bit 1) <- makeReg false
  reserveCounter :: Reg (Bit 16) <- makeReg 0
  reservation :: Wire (Bit 1) <- makeWire false

  always do
    if reservation.val then do
      reserveCounter <== 32
      reserved <== true
    else do
      reserveCounter <== reserved.val ? (reserveCounter.val - 1, 0)

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

  address :: Reg (Bit (AddrWidth p)) <- makeReg dontCare
  cap :: Reg TLPerm <- makeReg dontCare

  -- We use a wire in addition to `dataRamA.storeActiveBE` because in case
  -- of an atomic operation, the write is delayed by at least one cycle
  dirty_w :: Wire (Bit 1) <- makeWire dataRamA.storeActiveBE

  -- Ensure a `probe` doesn't start at the same time than a `match`
  exec_w :: Wire (Bit 1) <- makeWire false

  let canExecOp :: Bit 1 =
        inv (dataQueue.canDeq .&&. dataQueue.first.valid)

  let execOp :: Bit w -> Action () = \ way -> do
        if request.val `isTagged` #LoadR then do
          reservation <== true
        else do
          reserved <== false

        when (request.val `isTagged` #Load .||. request.val `isTagged` #LoadR) do
          dataRamA.loadBE (way # index.val # offset.val)
          dataQueue.enq none
        when (request.val `isTagged` #Atomic) do
          dataQueue.enq (some (untag #Atomic request.val, way # index.val # offset.val))
          dataRamA.loadBE (way # index.val # offset.val)
          dirty_w <== true
        when (request.val `isTagged` #Store) do
          let (mask,lane) = untag #Store request.val
          dataRamA.storeBE (way # index.val # offset.val) mask lane
        when (request.val `isTagged` #StoreC) do
          dynamicAssert (scResponseQ.notFull) "enq into a full queue"
          scResponseQ.enq reserved.val
          when (reserved.val) do
            let (mask,lane) = untag #StoreC request.val
            dataRamA.storeBE (way # index.val # offset.val) mask lane

  let baseAddr :: Bit kw -> Bit (AddrWidth p) = \ key ->
        encode @p @kw @iw @ow key index.val 0

  always do
    when (state.read 0 === st_release .&&. releaseM.ack.canPeek .&&. acquireM.canAcquire) do
      acquireM.acquireBlock (tag #NtoT ()) (way.val # index.val # 0) (baseAddr key.val)
      numRelease <== numRelease.val + 1
      state.write 0 st_acquire
      releaseM.ack.consume

    when (state.read 0 === st_acquire .&&. acquireM.canAcquireAck .&&. canExecOp) do
      storeListRAM keyRam way.val index.val key.val
      numAcquire <== numAcquire.val + 1

      execOp way.val
      exec_w <== true
      state.write 0 st_idle
      perm <- acquireM.acquireAck
      storeListRAM permRam way.val index.val (dirty_w.val ? (dirty,perm))

  always do
    let canProbe =
          probeM.start.canPeek
          .&&. inv exec_w.val
          .&&. inv dataQueue.canDeq
          .&&. reserveCounter.val === 0
          .&&. (state.read 1 === st_idle .||. state.read 1 === st_acquire)
    when canProbe do
      let (addr, perm) = probeM.start.peek
      --display "lookup probe 0x" (formatHex 0 addr) " " perm
      let (_,idx,_) = decode @p @kw @iw @ow addr
      state.write 1 st_probe_lookup
      nextState <== state.read 1
      address <== addr

      -- display source " probe " (formatHex 0 addr)

      sequence_ [do
          p.load idx
          t.load idx
          | (p,t) <- zip permRam keyRam]
      probeM.start.consume
      cap <== perm

      numProbe <== numProbe.val + 1

    when (state.read 0 === st_probe_lookup .&&. probeM.evict.canPut) do
      let (key, index, _) = decode @p @kw @iw @ow address.val
      state.write 0 st_probe_burst

      let hit =
            orList
              [key === t.out .&&. p.out .>. nothing
                | (t,p) <- zip keyRam permRam]

      let (way :: Bit w, perm) =
            selectDefault (0, nothing)
              [key === t.out .&&. p.out .>. nothing --> (constant i, p.out)
                | (t,p,i) <- zip3 keyRam permRam [0..]]

      --when (perm .>. nothing) do
      --  display "match probe 0x" (formatHex 0 address.val) " " perm

      when (perm === nothing) do
        probeM.evict.put (n2n, none)
      when (perm === branch) do
        if cap.val === nothing then do
          probeM.evict.put (b2n, none)
          storeListRAM permRam way index nothing
        else do
          probeM.evict.put (b2b, none)
      when (perm .>=. trunk) do
        when (cap.val === trunk) do
          probeM.evict.put (t2t, none)
        when (cap.val === branch) do
          probeM.evict.put (t2b, some $ way # index # 0)
          storeListRAM permRam way index branch
          reserved <== false
        when (cap.val === nothing) do
          probeM.evict.put (t2n, some $ way # index # 0)
          storeListRAM permRam way index nothing
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
          inv (dataQueue.canDeq .&&. dataQueue.first.valid)
          .&&. state.read 0 === st_lookup
          .&&. scResponseQ.notFull
          .&&. dataQueue.notFull
          .&&. acquireM.canAcquire
          .&&. releaseM.start.canPut
      , match= \ msb -> do
          numReq <== numReq.val + 1
          exec_w <== true
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
            --when (source .<. 2) do
            --  display source " hit " (formatHex 0 (baseAddr msb))
            execOp hitWay
            state.write 0 st_idle
            when dirty_w.val do
              storeListRAM permRam hitWay index.val dirty
            numHit <== numHit.val + 1
          else do
            way <== hitWay
            reserved <== false
            if outListRAM permRam hitWay =!= nothing .&&. outListRAM keyRam hitWay =!= msb then do
              let oldPerm = outListRAM permRam hitWay
              let oldKey = outListRAM keyRam hitWay
              --display "release " (formatHex 0 (baseAddr oldKey))

              -- writeback only if the choosen block is dirty
              when (oldPerm === dirty) do
                releaseM.start.put (t2n, baseAddr oldKey, some $ hitWay # index.val # 0)
              when (oldPerm === trunk) do
                releaseM.start.put (t2n, baseAddr oldKey, none)
              when (oldPerm === branch) do
                releaseM.start.put (b2n, baseAddr oldKey, none)

              --when (source .<. 2) do
              --  display source " release " (formatHex 0 (baseAddr oldKey))

              -- invalidate the permission of the choosen block
              storeListRAM permRam hitWay index.val nothing
              state.write 0 st_release
            else do
              --when (source .<. 2) do
              --  display source " acquire " (formatHex 0 (baseAddr msb))
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
            , peek= dataRamA.outBE}
      , stats=
        BCacheStats
          { numAcquire= numAcquire.val
          , numRelease= numRelease.val
          , numProbe= numProbe.val
          , numHit= numHit.val
          , numReq= numReq.val }}

makeBCacheCore :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , FShow atomic
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

testBCacheCore :: Bit 1 -> Module (Bit 1)
testBCacheCore _ = do
  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ _ -> 0
          , rootSink= \ _ -> 0
          , rootSource= \ x -> x === 0 ? (0,1)
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([master], [slave0,slave1]) <- makeTLXBar @1 @2 @(TLParams 32 4 4 8 8) xbarconfig

  -- A cache with atomic swap operation
  (cache0, master0) <- makeBCacheCore @1 @20 @6 @4 @(Bit 32) @(TLParams 32 4 4 8 8) 0 (\ x _ -> x)
  (cache1, master1) <- makeBCacheCore @1 @20 @6 @4 @(Bit 32) @(TLParams 32 4 4 8 8) 1 (\ x _ -> x)

  let bconfig =
        BroadcastConfig
          { sources= [0,1]
          , logSize= 6
          , baseSink= 0 }
  (slave, uncachedMaster) <- makeBroadcast @(TLParams 32 4 4 8 8) bconfig

  let ramconfig =
        TLRAMConfig
          { fileName= Just "Mem.hex"
          , lowerBound= 0x80000000
          , bypassChannelA= False
          , bypassChannelD= True
          , sink= 0 }
  uncachedSlave <- makeTLRAM @16 @(TLParams 32 4 4 8 0) ramconfig

  makeConnection uncachedMaster uncachedSlave

  makeConnection master slave
  makeConnection master0 slave0
  makeConnection master1 slave1

  out0 :: Reg (Bit 32) <- makeReg dontCare
  out1 :: Reg (Bit 32) <- makeReg dontCare

  count0 :: Reg (Bit 32) <- makeReg 0
  count1 :: Reg (Bit 32) <- makeReg 0

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  let loadWith cache out (key, idx, off) = do
        wait cache.canLookup
        action (cache.lookup idx off (item #Load))
        wait cache.canMatch
        action (cache.match key)
        wait cache.loadResponse.canPeek
        action do
          out <== cache.loadResponse.peek
          cache.loadResponse.consume

  let load0 = loadWith cache0 out0
  let load1 = loadWith cache1 out1

  let swapWith cache out (key, idx, off) val = do
        wait cache.canLookup
        action (cache.lookup idx off (tag #Atomic val))
        wait cache.canMatch
        action (cache.match key)
        wait cache.atomicResponse.canPeek
        action do
          out <== cache.atomicResponse.peek
          cache.atomicResponse.consume

  let swap0 = swapWith cache0 out0
  let swap1 = swapWith cache1 out1

  let storeWith cache out (key, idx, off) mask val = do
        wait cache.canLookup
        action (cache.lookup idx off (tag #Store (mask,val)))
        wait cache.canMatch
        action (cache.match key)

  let store0 = storeWith cache0 out0
  let store1 = storeWith cache1 out1

  let acquireWith cache out addr = do
        action (out <== 1)
        while (out.val === 1) do
          swapWith cache out addr 1

  let acquire0 = acquireWith cache0 out0
  let acquire1 = acquireWith cache1 out1

  let releaseWith cache out addr = do
        storeWith cache out addr ones 0

  let release0 = releaseWith cache0 out0
  let release1 = releaseWith cache1 out1

  let incrWith cache out (index :: Int) mutex addr = do
        acquireWith cache out mutex
        loadWith cache out addr
        action (display "[" index ", " cycle.val "] counter: " out.val)
        storeWith cache out addr ones (out.val + 1)
        releaseWith cache out mutex

  let incr0 = incrWith cache0 out0 0
  let incr1 = incrWith cache1 out1 1

  let delay = 100

  let mutex = (0,0,0)
  let var0 = (0,0,1)

  let prog0 = do
        while (count0.val =!= 0)  do
          action (count0 <== count0.val - 1)
          incr0 mutex var0
          sequence_ (replicate delay tick)

  let prog1 = do
        while (count1.val =!= 0)  do
          action (count1 <== count1.val - 1)
          incr1 mutex var0
          sequence_ (replicate delay tick)

  runStmt do
    store0 mutex ones 0x00
    store0 var0 ones 0x00
    action (count0 <== 10)
    action (count1 <== 10)
    par [prog0, prog1]
    load0 var0
    action (display "final value: " out0.val " at " cycle.val)
    action (count0 <== 10)
    prog0
    action finish

  return master.channelA.canPeek
