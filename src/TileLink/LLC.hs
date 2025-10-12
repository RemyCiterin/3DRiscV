module TileLink.LLC where

import Data.Proxy
import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Sharing
import Blarney.Arbiter
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.TypeFamilies
import Blarney.Utils
import Blarney.ADT

import TileLink.RAM
import TileLink.Types
import TileLink.Utils
import TileLink.AcquireRelease
import TileLink.Interconnect
import TileLink.Broadcast

--             hit
-- InputA ------------> Probe new addr
--         |
--         |    mis
--         +----------> Probe old addr

-- OffsetW must be 4 bytes: size of a cache line
type OffsetW = 4

-- IndexW + TagW must be 26
type IndexW = 8
type TagW = 18
type Ways = 4

-- Up to snoop targets
type OwnerW = 8

type Owner = Bit OwnerW

type Offset = Bit OffsetW
type Index = Bit IndexW
type Tag = Bit TagW
type Way = Bit (Log2Ceil Ways)

getIndex :: Bit 32 -> Index
getIndex addr = slice @(6+IndexW-1) @6 addr

getTag :: Bit 32 -> Tag
getTag addr = slice @31 @(32-TagW) addr

type ReqKind = Bit 1
reqChannelA, reqChannelC :: ReqKind
reqChannelA = 0
reqChannelC = 1


data TransactionQueue p =
  TransactionQueue
    { canEnq :: Bit 1
    , enq :: ChannelA p -> Action (Bit (SinkWidth p))
    -- ^ add a new transaction into the queue
    , search :: Bit (AddrWidth p) -> Bit 1
    -- ^ return if the transaction queue contains at least one acquire that
    --   match a given address
    , deq :: Bit (SinkWidth p) -> Action ()
    -- ^ remove (the unique) transaction that correspond to a given source
    } deriving(Generic)

makeTransactionQueue :: forall p.
  ( KnownTLParams p
  , AddrWidth p ~ 32 )
    => [Bit (SinkWidth p)] -> Module (TransactionQueue p)
makeTransactionQueue sinks = do
  let logSize = log2 (length sinks)
  liftNat logSize $ \(_ :: Proxy aw) -> do
    liftNat (2 ^ logSize) $ \(_ :: Proxy size) -> do
      addresses :: [Reg (Bit (AddrWidth p))] <- replicateM (2 ^ logSize) (makeReg dontCare)

      valid :: Ehr (Bit size) <- makeEhr 2 0
      head :: Reg (Bit aw) <- makeReg 0

      return
        TransactionQueue
          { canEnq= inv ((valid.read 0)!head.val)
          , enq= \ input -> do
            valid.write 0 (valid.read 0 .|. (1 .<<. head.val))
            addresses!head.val <== input.address
            head <== head.val + 1
            return (sinks!head.val)
          , search= \ addr ->
            orList
              [ v .&&. getIndex addr === getIndex a.val
                | (a, v) <- zip addresses (toBitList (valid.read 0)) ]
          , deq= \ sink -> do
            let invalidated :: Bit size =
                  fromBitList (map (===sink) sinks)
            valid.write 1 (valid.read 1 .&. inv invalidated) }

type RequestKind =
  TaggedUnion
    [ "RequestA" ::: ()
    , "RequestC" ::: () ]

data LLCConfig p =
  LLCConfig
    { sources :: [Bit (SourceWidth p)]
    -- ^ sources that we need to invalidate to perform an acquire request
    , baseSink :: Bit (SinkWidth p)
    -- ^ lowest sink ID of the range of sinks allocated to the LLC
    , logNumSink :: Int }


-- Last Level Cache
--  - Non blocking
--  - Accecpt up to one request per "set" at any times:
makeLLC :: forall p p'.
  ( p' ~ TLParams (AddrWidth p) (LaneWidth p) (SizeWidth p) (SinkWidth p) 0
  , AddrWidth p ~ 32
  , LaneWidth p ~ 4
  , KnownTLParams p )
  => LLCConfig p -> Module (TLSlave p, TLMaster p')
makeLLC config = do
  randomWay :: Reg Way <- makeReg 0
  always (randomWay <== randomWay.val + 1)

  [memWrArbiter, cpuWrArbiter] <- makeStaticArbiter 2
  [memRdArbiter, cpuRdArbiter] <- makeStaticArbiter 2

  slaveA :: Queue (ChannelA p') <- makeQueue
  slaveD :: Queue (ChannelD p') <- makeQueue

  queueA :: Queue (ChannelA p) <- makeQueue
  queueB :: Queue (ChannelB p) <- makeQueue
  queueC :: Queue (ChannelC p) <- makeQueue
  queueD :: Queue (ChannelD p) <- makeQueue
  queueE :: Queue (ChannelE p) <- makeQueue
  let sourceE = toSource queueE
  let sourceA = toSource queueA
  let sourceC = toSource queueC
  let sinkB = toSink queueB
  let sinkD = toSink queueD

  transactions :: TransactionQueue p <-
    makeTransactionQueue [ config.baseSink + lit i | i <- [0..2 ^ config.logNumSink - 1] ]

  retryQ :: Queue (ChannelA p) <- makeSizedQueue 4

  tags :: [RAM Index Tag] <- replicateM (valueOf @Ways) makeDualRAMForward
  perms :: [RAM Index TLPerm] <- replicateM (valueOf @Ways) makeDualRAMForward
  owners :: [RAM Index Owner] <- replicateM (valueOf @Ways) makeDualRAMForward

  stage1 :: Queue RequestKind <- makePipelineQueue 1
  sink1 :: Reg (Bit (SinkWidth p)) <- makeReg dontCare
  requestA1 :: Reg (ChannelA p) <- makeReg dontCare
  requestC1 :: Reg (ChannelC p) <- makeReg dontCare

  -------------------------------------------------------------------------------------------
  -- Stage 1: Lookup
  -------------------------------------------------------------------------------------------
  always do
    -- Stage 1 ready
    when (stage1.notFull) do
      if sourceC.canPeek then do
        -- We received a ProbeAck* response or a Release* request
        sequence_ [owner.load (getIndex sourceC.peek.address) | owner <- owners]
        sequence_ [perm.load (getIndex sourceC.peek.address) | perm <- perms]
        sequence_ [tag.load (getIndex sourceC.peek.address) | tag <- tags]
        stage1.enq (item #RequestC)
        requestC1 <== sourceC.peek

      else if retryQ.notFull .&&. sourceA.canPeek .&&. transactions.canEnq then do
        -- We received an Acquire* request
        sequence_ [owner.load (getIndex sourceA.peek.address) | owner <- owners]
        sequence_ [perm.load (getIndex sourceA.peek.address) | perm <- perms]
        sequence_ [tag.load (getIndex sourceA.peek.address) | tag <- tags]

        -- slove acquire hazard
        if transactions.search sourceA.peek.address then do
          retryQ.enq sourceA.peek
        else do
          sink <- transactions.enq sourceA.peek
          stage1.enq (item #RequestA)
          requestA1 <== sourceA.peek
          sink1 <== sink

      else if retryQ.canDeq .&&. transactions.canEnq then do
        -- Retry a previous Acquire* request
        sequence_ [owner.load (getIndex retryQ.first.address) | owner <- owners]
        sequence_ [perm.load (getIndex retryQ.first.address) | perm <- perms]
        sequence_ [tag.load (getIndex retryQ.first.address) | tag <- tags]

        when (inv (transactions.search retryQ.first.address)) do
          sink <- transactions.enq retryQ.first
          stage1.enq (item #RequestA)
          requestA1 <== retryQ.first
          sink1 <== sink
          retryQ.deq

      else if sourceE.canPeek then do
        -- We received a GrantAck response
        transactions.deq sourceE.peek.sink

      else do
        pure ()

  -------------------------------------------------------------------------------------------
  -- Stage 1: Matching
  -------------------------------------------------------------------------------------------

  always do
    when stage1.canDeq do
      let kind = stage1.first

      -- Address of the request
      let address =
            kind `isTagged` #RequestC ? (requestC1.val.address, requestA1.val.address)

      let tag = getTag address

      let hits :: Bit Ways =
            fromBitList
              [ p.out =!= nothing .&&. t.out === tag
                | (t,p) <- zip tags perms ]

      let way :: Way =
            selectDefault randomWay.val
              [ hit --> lit i
                | (hit,i) <- zip (toBitList hits) [0..] ]

      if kind `isTagged` #RequestC then do
        cpuWrArbiter.request

      else do
        pure ()

      when cpuWrArbiter.grant do
        -- Write to the data ram
        stage1.deq

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
      , channelE= nullSource })
