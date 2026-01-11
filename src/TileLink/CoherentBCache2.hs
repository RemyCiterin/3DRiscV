module TileLink.CoherentBCache2
  ( BCacheRequest2
  , BCacheCore2(..)
  , BCacheStats2(..)
  , makeBCacheCore2With
  , makeBCacheCore2
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

storeListRAMBE ::
  (KnownNat a, KnownNat b, KnownNat (8*b), KnownNat idx) =>
    [RAMBE a b] -> Bit idx -> Bit a -> Bit b -> Bit (8*b) -> Action ()
storeListRAMBE rams index k m v =
  sequence_ [when (constant i === index) do r.storeBE k m v | (r,i) <- zip rams [0..]]

loadListRAMBE ::
  (KnownNat a, KnownNat b, KnownNat (8*b), KnownNat idx) =>
    [RAMBE a b] -> Bit idx -> Bit a -> Action ()
loadListRAMBE rams index k =
  sequence_ [when (constant i === index) do r.loadBE k | (r,i) <- zip rams [0..]]

outListRAMBE ::
  (KnownNat a, KnownNat b, KnownNat (8*b), KnownNat idx) =>
    [RAMBE a b] -> Bit idx -> (Bit (8*b))
outListRAMBE rams index =
  select [constant i === index --> r.outBE | (r,i) <- zip rams [0..]]


type BCacheRequest2 atomic p =
  TaggedUnion
    [ "Load" ::: ()
    , "LoadR" ::: ()
    , "Atomic" ::: atomic
    , "StoreC" ::: (Bit (LaneWidth p), Bit (8 * LaneWidth p))
    , "Store" ::: (Bit (LaneWidth p), Bit (8 * LaneWidth p))]

formatBCacheRequest2 :: forall atomic p. (KnownTLParams p, FShow atomic, Bits atomic) =>
  BCacheRequest2 atomic p -> Format
formatBCacheRequest2 request =
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
  (Bits atomic, KnownTLParams p) => BCacheRequest2 atomic p -> Bit 1
needPermTrunk req = inv (req `isTagged` #Load)

satisfyPerms :: forall atomic p.
  (Bits atomic, KnownTLParams p) => BCacheRequest2 atomic p -> TLPerm -> Bit 1
satisfyPerms req perm =
  perm =!= nothing .&&. inv (perm === branch .&&. needPermTrunk @atomic @p req)

-- Operations are performed in order here, no NACK at matching stage
data BCacheCore2 kw iw ow atomic p =
  BCacheCore2
    { canLookup :: Bit 1
    , lookup :: Bit iw -> Bit ow -> BCacheRequest2 atomic p -> Action ()
    , canMatch :: Bit 1
    , match :: Bit kw -> Action ()
    , abort :: Action ()
    , response :: Source (Bit (8*LaneWidth p))
    , stats :: BCacheStats2 }

data BCacheStats2 =
  BCacheStats2
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
st_probe_burst= 5
st_exec_after_acquire= 6

-- Given the offset of a word in a cache line and the index/key of the cache line, return the
-- address of the word
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

-- Given the address of a word, return the offset of the word in a cache line, and the index/key of
-- the cache line in memory
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
--
--  Pipeline Flow:
--                             +-----------------------------------------> response
--                             |                                              ^
--                             |                                              |
--  idle ----> lookup ----> matching ----> release ----> acquire ----> exec_after_acquire
--                             |                            ^
--                             |                            |
--                             +----------------------------+
--
makeBCacheCore2With :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , FShow atomic
  , KnownNat (iw+ow)
  , KnownNat (w+(iw+ow))
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
    => Bit (SourceWidth p)
    -> TLSlave p
    -> (atomic -> Bit (8 * LaneWidth p) -> Bit (8 * LaneWidth p))
    -> Module (BCacheCore2 kw iw ow atomic p)
makeBCacheCore2With source slave execAtomic = do
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

  responseQ :: Queue (Bit (8 * LaneWidth p)) <- makePipelineQueue 1

  state :: Ehr BCacheState <- makeEhr 2 st_idle
  nextState :: Reg BCacheState <- makeReg dontCare

  let ways = 2^(valueOf @w)
  -- Permissions of each cache lines
  permRam :: [RAM (Bit iw) TLPerm] <- replicateM ways makeDualRAMForward

  -- Keys of each cache line: To each address, we associate a `key`, an `index` and an `offset`
  --  - The `offset` correspond to the offset of the word in a cache line, so it correspond to the
  --    less significant bits of the address
  --  - The `index` is used to find the `seŧ` of a given cache line, knowning that each set can
  --    represent up to `2 ** w` different lines, the index of a cache line in a set is named it's
  --    `way|
  --  - Then the `key` is used to differentiate the different lines in a given `set`: for all the
  --    lines with the same `index`, we store it's most significant bits to remember it's full
  --    address
  keyRam :: [RAM (Bit iw) (Bit kw)] <- replicateM ways makeDualRAMForward

  -- Create a multi-banked data ram (one bank per way):
  --  - `dataRamA` is used by the CPU oriented interface of the cache to load all the data
  --    corresponding to a given pair `(index, offset)` at the lookup stage, then the `matching`
  --    stage can use those data to perform the operation. The same thing must be done after an
  --    `acquire`
  --  - `dataRamB` is used by the memory oriented interface of the cache to evict and refill lines
  --    from the main memory
  (dataRamA,dataRamB) :: ([RAMBE (iw+ow) (LaneWidth p)], RAMBE (w+(iw+ow)) (LaneWidth p)) <- do
        -- Create the ram banks
        rams :: [RAMBE (iw+ow) (LaneWidth p)] <- replicateM ways makeDualRAMForwardBE

        -- share the ram banks between the cpu and memory interfaces
        sharedRams <- mapM (makeSharedRAMBE 2) rams
        let cpuRams :: [RAMBE (iw+ow) (LaneWidth p)] = [c | [c,m] <- sharedRams]
        let memRams :: [RAMBE (iw+ow) (LaneWidth p)] = [m | [c,m] <- sharedRams]
        lastWay :: Reg (Bit w) <- makeReg dontCare
        let memRam =
              RAMBE
                { loadBE= \ addr -> do
                  let (way,idx) :: (Bit w, Bit (iw+ow)) = split addr
                  loadListRAMBE memRams way idx
                  lastWay <== way
                , storeBE= \ addr mask lane -> do
                  let (way,idx) :: (Bit w, Bit (iw+ow)) = split addr
                  storeListRAMBE memRams way idx mask lane
                , outBE= outListRAMBE memRams lastWay.val
                , storeActiveBE= orList [m.storeActiveBE | m <- memRams]}
        return (cpuRams, memRam)

  let logSize :: Bit (SizeWidth p) =
        constant $ toInteger $ valueOf @(AddrWidth p) - valueOf @kw - valueOf @iw

  -- A finite state machine in charge of loading cache lines from the main memory to the local data
  acquireM <- makeAcquireMaster @(w+(iw+ow)) @p source logSize putArbiter dataRamB slave

  -- A finite state machine in charge of evicting cache lines from the local data array to the main
  -- memory, either because of a release (volontary eviction), or because we receive an invalidation
  -- request from the cache controller
  (releaseM, probeM) <- makeReleaseMaster @(w+(iw+ow)) @p source logSize getArbiter dataRamB slave

  -- Random replacement policy
  randomWay :: Reg (Bit w) <- makeReg dontCare
  always do randomWay <== randomWay.val + 1

  -- Current request informations, used by the matching and acquire/release stages to known with
  -- transactions to perform
  request :: Reg (BCacheRequest2 atomic p) <- makeReg dontCare
  offset :: Reg (Bit ow) <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  key :: Reg (Bit kw) <- makeReg dontCare
  way :: Reg (Bit w) <- makeReg dontCare

  -- Informations relative to the current probe request we receive from the cache controller
  probeAddress :: Reg (Bit (AddrWidth p)) <- makeReg dontCare
  probeCap :: Reg TLPerm <- makeReg dontCare

  -- The current cache line is written, the permission must increase from trunk to dirty
  let dirty_w :: Bit 1 = orList [d.storeActiveBE | d <- dataRamA]

  -- No probe request can start at this cycle because some ressources are busy
  probeBlocked :: Wire (Bit 1) <- makeWire false

  -- Execute an operation on a cache line given a `way`, it use the value of `index.val` and
  -- `offset.val` to find the location of the access in the local data array. The data must already
  -- be loaded in the output register of `dataRamA[way]`
  let execOp :: Bit w -> Action () = \ way -> do
        if request.val `isTagged` #LoadR then do
          reservation <== true
        else do
          reserved <== false

        when (request.val `isTagged` #Load .||. request.val `isTagged` #LoadR) do
          responseQ.enq (outListRAMBE dataRamA way)

        when (request.val `isTagged` #Atomic) do
          let out = outListRAMBE dataRamA way
          let lane = execAtomic (untag #Atomic request.val) out
          storeListRAMBE dataRamA way (index.val # offset.val) ones lane
          responseQ.enq out

        when (request.val `isTagged` #Store) do
          let (mask,lane) = untag #Store request.val
          storeListRAMBE dataRamA way (index.val # offset.val) mask lane
          responseQ.enq (outListRAMBE dataRamA way)

        when (request.val `isTagged` #StoreC) do
          responseQ.enq (reserved.val ? (0, 1))
          when (reserved.val) do
            let (mask,lane) = untag #StoreC request.val
            storeListRAMBE dataRamA way (index.val # offset.val) mask lane

  let baseAddr :: Bit kw -> Bit (AddrWidth p) = \ key ->
        encode @p @kw @iw @ow key index.val 0

  always do
    -- We finished to evict a line => we must acquire the new line
    when (state.read 0 === st_release .&&. releaseM.ack.canPeek .&&. acquireM.canAcquire) do
      acquireM.acquireBlock n2t (way.val # index.val # 0) (baseAddr key.val)
      numRelease <== numRelease.val + 1
      state.write 0 st_acquire
      releaseM.ack.consume

    -- We receive the new cache line => we must update the key and permission arrays, and load the
    -- requested data in `dataRamA` to perform the operation at the next cycle
    when (state.read 0 === st_acquire .&&. acquireM.canAcquireAck) do
      storeListRAM keyRam way.val index.val key.val
      numAcquire <== numAcquire.val + 1

      probeBlocked <== true
      perm <- acquireM.acquireAck
      state.write 0 st_exec_after_acquire
      loadListRAMBE dataRamA way.val (index.val # offset.val)
      storeListRAM permRam way.val index.val perm

    -- Perform the blocked operation after waiting for the line to be loaded from main memory
    when (state.read 0 === st_exec_after_acquire .&&. responseQ.notFull) do
      when dirty_w $ storeListRAM permRam way.val index.val dirty
      probeBlocked <== true
      state.write 0 st_idle
      execOp way.val

  always do
    let canProbe =
          probeM.start.canPeek
          .&&. inv probeBlocked.val
          .&&. reserveCounter.val === 0
          .&&. (state.read 1 === st_idle .||. state.read 1 === st_acquire)
    when canProbe do
      let (addr, perm) = probeM.start.peek
      let (_,idx,_) = decode @p @kw @iw @ow addr
      state.write 1 st_probe_lookup
      nextState <== state.read 1
      probeAddress <== addr

      sequence_ [do
          p.load idx
          t.load idx
          | (p,t) <- zip permRam keyRam]
      probeM.start.consume
      probeCap <== perm

      numProbe <== numProbe.val + 1

    when (state.read 0 === st_probe_lookup .&&. probeM.evict.canPut) do
      let (key, index, _) = decode @p @kw @iw @ow probeAddress.val
      state.write 0 st_probe_burst

      let hit =
            orList
              [key === t.out .&&. p.out .>. nothing
                | (t,p) <- zip keyRam permRam]

      let (way :: Bit w, perm) =
            selectDefault (0, nothing)
              [key === t.out .&&. p.out .>. nothing --> (constant i, p.out)
                | (t,p,i) <- zip3 keyRam permRam [0..]]

      when (perm === nothing) do
        probeM.evict.put (n2n, none)
      when (perm === branch) do
        if probeCap.val === nothing then do
          probeM.evict.put (b2n, none)
          storeListRAM permRam way index nothing
        else do
          probeM.evict.put (b2b, none)
      when (perm .>=. trunk) do
        when (probeCap.val === trunk) do
          probeM.evict.put (t2t, none)
        when (probeCap.val === branch) do
          probeM.evict.put (t2b, some $ way # index # 0)
          storeListRAM permRam way index branch
          reserved <== false
        when (probeCap.val === nothing) do
          probeM.evict.put (t2n, some $ way # index # 0)
          storeListRAM permRam way index nothing
          reserved <== false

    when (state.read 0 === st_probe_burst .&&. probeM.ack.canPeek) do
      state.write 0 nextState.val
      probeM.ack.consume

  let loockupBlocked = probeM.start.canPeek .&&. reserveCounter.val === 0

  return
    BCacheCore2
      { canLookup= state.read 1 === st_idle .&&. inv loockupBlocked
      , lookup= \ idx off req -> do
          dynamicAssert (state.read 1 === st_idle) "lookup with an unexpected state"
          request <== req
          offset <== off
          index <== idx

          state.write 1 st_lookup
          sequence_ [do
              p.load idx
              t.load idx
              d.loadBE (idx # off)
              | (p,t,d) <- zip3 permRam keyRam dataRamA]
      , canMatch=
          state.read 0 === st_lookup
          .&&. responseQ.notFull
          .&&. acquireM.canAcquire
          .&&. releaseM.start.canPut
      , match= \ msb -> do
          probeBlocked <== true
          numReq <== numReq.val + 1
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
            let lane = outListRAMBE dataRamA hitWay
            execOp hitWay
            state.write 0 st_idle
            when dirty_w do
              storeListRAM permRam hitWay index.val dirty
            numHit <== numHit.val + 1
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
      , response= toSource responseQ
      , stats=
        BCacheStats2
          { numAcquire= numAcquire.val
          , numRelease= numRelease.val
          , numProbe= numProbe.val
          , numHit= numHit.val
          , numReq= numReq.val }}

makeBCacheCore2 :: forall w kw iw ow atomic p.
  ( Bits atomic
  , KnownTLParams p
  , KnownNat iw
  , KnownNat ow
  , KnownNat kw
  , KnownNat w
  , FShow atomic
  , KnownNat (iw+ow)
  , KnownNat (w+(iw+ow))
  , KnownNat (Log2 (LaneWidth p))
  , KnownNat (kw+(iw+(ow+Log2 (LaneWidth p)))))
    => Bit (SourceWidth p)
    -> (atomic -> Bit (8 * LaneWidth p) -> Bit (8 * LaneWidth p))
    -> Module (BCacheCore2 kw iw ow atomic p, TLMaster p)
makeBCacheCore2 source execAtomic = do
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

  ifc <- makeBCacheCore2With @w @kw @iw @ow @atomic @p source slave execAtomic
  return (ifc,master)
