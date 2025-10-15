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

type AddrW = 32

-- IndexW + TagW must be 26
type IndexW = 8
type TagW = 18
type Ways = 4

-- Up to snoop targets
type OwnerW = 8

type Owner = Bit OwnerW

type Addr = Bit AddrW
type Offset = Bit OffsetW
type Index = Bit IndexW
type Tag = Bit TagW
type Way = Bit (Log2Ceil Ways)

getIndex :: Addr -> Index
getIndex addr = slice @(6+IndexW-1) @6 addr

getTag :: Addr -> Tag
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
  let sinkD = toSink queueD

  probeM <- makeProbeFSM config.sources (toSink queueB)

  transactions :: TransactionQueue p <-
    makeTransactionQueue [ config.baseSink + lit i | i <- [0..2 ^ config.logNumSink - 1] ]

  retryQ :: Queue (ChannelA p) <- makeSizedQueue 4

  (tagsA, tagsC) <- makeDualPortdeRAM @Tag
  (permsA, permsC) <- makeDualPortdeRAM @TLPerm
  (ownersA, ownersC) <- makeDualPortdeRAM @Owner

  stage1A :: Queue (Bit (SinkWidth p), ChannelA p) <- makePipelineQueue 2
  stage1C :: Queue (ChannelC p) <- makePipelineQueue 2

  -------------------------------------------------------------------------------------------
  -- Stage 1: Lookup
  -------------------------------------------------------------------------------------------
  always do
    if sourceE.canPeek then do
      -- We received a GrantAck response
      transactions.deq sourceE.peek.sink
      sourceE.consume

    else if stage1C.notFull .&&. sourceC.canPeek then do
      -- We received a ProbeAck* response or a Release* request
      sequence_ [owner.load (getIndex sourceC.peek.address) | owner <- ownersC]
      sequence_ [perm.load (getIndex sourceC.peek.address) | perm <- permsC]
      sequence_ [tag.load (getIndex sourceC.peek.address) | tag <- tagsC]
      stage1C.enq sourceC.peek
      sourceC.consume

    else when (stage1A.notFull .&&. transactions.canEnq) do

      if retryQ.notFull .&&. sourceA.canPeek then do
        -- We received an Acquire* request
        sequence_ [owner.load (getIndex sourceA.peek.address) | owner <- ownersA]
        sequence_ [perm.load (getIndex sourceA.peek.address) | perm <- permsA]
        sequence_ [tag.load (getIndex sourceA.peek.address) | tag <- tagsA]

        -- slove acquire hazard
        if transactions.search sourceA.peek.address then do
          retryQ.enq sourceA.peek
        else do
          sink <- transactions.enq sourceA.peek
          stage1A.enq (sink, sourceA.peek)

        sourceA.consume

      else if retryQ.canDeq then do
        -- Retry a previous Acquire* request
        sequence_ [owner.load (getIndex retryQ.first.address) | owner <- ownersA]
        sequence_ [perm.load (getIndex retryQ.first.address) | perm <- permsA]
        sequence_ [tag.load (getIndex retryQ.first.address) | tag <- tagsA]

        when (inv (transactions.search retryQ.first.address)) do
          sink <- transactions.enq retryQ.first
          stage1A.enq (sink, retryQ.first)
          retryQ.deq

      else do
        pure ()

  -------------------------------------------------------------------------------------------
  -- Stage 1 C: Matching channel C inputs
  -------------------------------------------------------------------------------------------

  always do
    when stage1C.canDeq do
      let req = stage1C.first

      let tag = getTag req.address

      let hits :: Bit Ways =
            fromBitList
              [ p.out =!= nothing .&&. t.out === tag
                | (t,p) <- zip tagsC permsC ]

      let way :: Way =
            selectDefault randomWay.val
              [ hit --> lit i
                | (hit,i) <- zip (toBitList hits) [0..] ]

      cpuWrArbiter.request
      when cpuWrArbiter.grant do
        -- Write to the data ram
        stage1C.deq

  -------------------------------------------------------------------------------------------
  -- Stage 1 A: Matching channel A inputs
  -------------------------------------------------------------------------------------------

  always do
    when (stage1A.canDeq .&&. probeM.canPut) do
      let (sink, req) = stage1A.first

      let tag = getTag req.address

      let hits :: Bit Ways =
            fromBitList
              [ p.out =!= nothing .&&. t.out === tag
                | (t,p) <- zip tagsA permsA ]

      let way :: Way =
            selectDefault randomWay.val
              [ hit --> lit i
                | (hit,i) <- zip (toBitList hits) [0..] ]

      -- TODO: send the correct probe requests
      probeM.put (dontCare, dontCare, dontCare)
      stage1A.deq

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
  where
    makeDualPortdeRAM :: forall a. Bits a => Module ([RAM Index a], [RAM Index a])
    makeDualPortdeRAM = do
      ram :: [RAM Index a] <- replicateM (valueOf @Ways) makeDualRAMForward
      portA <- mapM makeSafeRAM ram
      portB <- mapM makeSafeRAM ram
      return (portA, portB)

    makeProbeFSM ::
      [Bit (SourceWidth p)]
      -> Sink (ChannelB p)
      -> Module (Sink (OpcodeB, Addr, Owner))
    makeProbeFSM sources sinkB = do
      opcode :: Reg OpcodeB <- makeReg dontCare
      address :: Reg Addr <- makeReg dontCare
      owners :: Reg Owner <- makeReg 0

      always do
        when (owners.val =!= 0 .&&. sinkB.canPut) do
          let choosen = firstHot owners.val
          let source = select [ cond --> src | (cond,src) <- zip (toBitList choosen) sources ]
          owners <== owners.val .&. inv choosen
          sinkB.put
            ChannelB
              { address= address.val
              , opcode= opcode.val
              , size= 6
              , source }

      return
        Sink
          { canPut= owners.val === 0
          , put= \ (op,addr,oneHot) -> do
              owners <== oneHot
              address <== addr
              opcode <== op }

