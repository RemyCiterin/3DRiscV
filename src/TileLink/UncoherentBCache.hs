module TileLink.UncoherentBCache
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
import Blarney.TaggedUnion

import TileLink.Types
import TileLink.Utils
import TileLink.GetPut

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

idle, reading, acquire, release :: Bit 2
idle = 0
reading = 1
acquire= 2
release = 3

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

  state :: Ehr (Bit 2) <- makeEhr 2 idle

  let ways = 2^(valueOf @w)
  validRam :: [RAM (Bit iw) (Bit 1)] <- replicateM ways makeDualRAMForward
  dirtyRam :: [RAM (Bit iw) (Bit 1)] <- replicateM ways makeDualRAMForward
  keyRam :: [RAM (Bit iw) (Bit kw)] <- replicateM ways makeDualRAMForward
  [dataRamA,dataRamB] <- makeDualRAMForwardBE >>= makeSharedRAMBE 2

  putM <- makePutMaster @(w+(iw+ow)) @p source putArbiter dataRamB slave
  getM <- makeGetMaster @(w+(iw+ow)) @p source getArbiter dataRamB slave

  randomWay :: Reg (Bit w) <- makeReg dontCare
  always do randomWay <== randomWay.val + 1

  request :: Reg (BCacheRequest atomic p) <- makeReg dontCare
  offset :: Reg (Bit ow) <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  key :: Reg (Bit kw) <- makeReg dontCare
  way :: Reg (Bit w) <- makeReg dontCare

  let execOp :: Bit w -> Action () = \ way -> do
        reserved <== request.val `is` #LoadR
        when (request.val `is` #Load .||. request.val `is` #LoadR) do
          dataRamA.loadBE (way # index.val # offset.val)
          dataQueue.enq none
        when (request.val `is` #Atomic) do
          dataQueue.enq (some (untag #Atomic request.val, way # index.val # offset.val))
          dataRamA.loadBE (way # index.val # offset.val)
          storeListRAM dirtyRam way index.val true
        when (request.val `is` #Store) do
          let (mask,lane) = untag #Store request.val
          dataRamA.storeBE (way # index.val # offset.val) mask lane
          storeListRAM dirtyRam way index.val true
        when (request.val `is` #StoreC) do
          dynamicAssert (scResponseQ.notFull) "enq into a full queue"
          scResponseQ.enq reserved.val
          when (reserved.val) do
            let (mask,lane) = untag #Store request.val
            dataRamA.storeBE (way # index.val # offset.val) mask lane
            storeListRAM dirtyRam way index.val true

  let address :: Bit kw -> Bit (AddrWidth p) = \ key ->
        cast (key # index.val # (0 :: Bit ow) # (0 :: Bit (Log2 (LaneWidth p))))

  let logSize :: Bit (SizeWidth p) =
        constant $ toInteger $ valueOf @(AddrWidth p) - valueOf @kw - valueOf @iw

  always do
    when (state.read 0 === release .&&. putM.canPutAck .&&. getM.canGet) do
      getM.get (way.val # index.val # 0) (address key.val) logSize
      --display "acquire block: 0x" (formatHex 0 (address key.val))
      state.write 0 acquire
      putM.putAck

    when (state.read 0 === acquire .&&. getM.canGetAck) do
      state.write 0 idle
      execOp way.val
      getM.getAck

  return
    BCacheCore
      { canLookup= state.read 1 === idle
      , lookup= \ idx off req -> do
          dynamicAssert (state.read 1 === idle) "lookup with an unexpected state"
          request <== req
          offset <== off
          index <== idx

          state.write 1 reading
          sequence_ [do
              v.load idx
              d.load idx
              t.load idx
              | (v,d,t) <- zip3 validRam dirtyRam keyRam]
      , canMatch=
          inv (dataQueue.canDeq .&&. dataQueue.first.valid .&&. request.val `is` #Store)
          .&&. inv (dataQueue.canDeq .&&. dataQueue.first.valid .&&. request.val `is` #StoreC)
          .&&. state.read 0 === reading
          .&&. scResponseQ.notFull
          .&&. dataQueue.notFull
          .&&. getM.canGet
          .&&. putM.canPut
      , match= \ msb -> do
          dynamicAssert (state.read 0 === reading) "matching with an unexpected state"
          key <== msb

          let hit = orList [v.out .&&. msb === t.out | (t,v) <- zip keyRam validRam]

          if hit then do
            let way :: Bit w =
                  select
                    [v.out .&&. msb === t.out --> constant i
                      | (t,v,i) <- zip3 keyRam validRam [0..]]

            state.write 0 idle
            execOp way
          else do
            reserved <== false
            way <== randomWay.val
            storeListRAM keyRam randomWay.val index.val msb
            storeListRAM validRam randomWay.val index.val true
            storeListRAM dirtyRam randomWay.val index.val false
            if outListRAM dirtyRam randomWay.val then do
              state.write 0 release
              let key = outListRAM keyRam randomWay.val
              putM.put (randomWay.val # index.val # 0) (address key) logSize
            else do
              state.write 0 acquire
              getM.get (randomWay.val # index.val # 0) (address msb) logSize

          return ()
      , abort= state.write 0 idle
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
