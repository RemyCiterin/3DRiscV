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
st_idle, st_lookup, st_acquire, st_release :: BCacheState
st_idle = 0
st_lookup= 1
st_acquire= 2
st_release= 3

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

  let address :: Bit kw -> Bit (AddrWidth p) = \ key ->
        cast (key # index.val # (0 :: Bit ow) # (0 :: Bit (Log2 (LaneWidth p))))

  always do
    when (state.read 0 === st_release .&&. releaseM.ack.canPeek .&&. acquireM.canAcquire) do
      acquireM.acquireBlock (tag #NtoT ()) (way.val # index.val # 0) (address key.val)
      --display "acquire block: 0x" (formatHex 0 (address key.val))
      state.write 0 st_acquire
      releaseM.ack.consume

    when (state.read 0 === st_acquire .&&. acquireM.canAcquireAck) do
      storeListRAM keyRam way.val index.val key.val

      execOp way.val
      state.write 0 st_idle
      perm <- acquireM.acquireAck
      storeListRAM permRam way.val index.val (dataRamA.storeActiveBE ? (dirty,perm))

  return
    BCacheCore
      { canLookup= state.read 1 === st_idle
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

          if hit then do
            let way :: Bit w =
                  select
                    [msb === t.out --> constant i
                      | (t,i) <- zip keyRam [0..]]

            execOp way
            state.write 0 st_idle
            when (dataRamA.storeActiveBE) do
              storeListRAM permRam way index.val dirty
          else do
            reserved <== false
            way <== randomWay.val
            storeListRAM permRam randomWay.val index.val nothing
            if outListRAM permRam randomWay.val =!= nothing then do
              state.write 0 st_release
              let key = outListRAM keyRam randomWay.val
              when (outListRAM permRam randomWay.val === dirty) do
                releaseM.start.put (t2n, address key, some $ randomWay.val # index.val # 0)
              when (outListRAM permRam randomWay.val === trunk) do
                releaseM.start.put (t2n, address key, none)
              when (outListRAM permRam randomWay.val === branch) do
                releaseM.start.put (b2n, address key, none)
            else do
              state.write 0 st_acquire
              acquireM.acquireBlock
                (needPermTrunk @atomic @p request.val ? (n2t, n2b))
                (randomWay.val # index.val # 0)
                (address msb)

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
  queueD <- makeQueue

  let slave =
        TLSlave
          { channelA= toSink queueA
          , channelB= nullSource
          , channelC= nullSink
          , channelD= toSource queueD
          , channelE= nullSink }
  let master =
        TLMaster
          { channelA= toSource queueA
          , channelB= nullSink
          , channelC= nullSource
          , channelD= toSink queueD
          , channelE= nullSource }

  ifc <- makeBCacheCoreWith @w @kw @iw @ow @atomic @p source slave execAtomic
  return (ifc,master)
