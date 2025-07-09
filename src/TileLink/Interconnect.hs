module TileLink.Interconnect where

import Blarney
import Blarney.Connectable
import Blarney.SourceSink
import Blarney.Sharing
import Blarney.Queue

import TileLink.Types
import TileLink.Utils

import Data.Proxy

-- interconnect only for the D channel: release ack, and grant operations need to put data
-- into the same channel
makeSharedBurstSink :: forall p a.
  KnownTLParams p
    => (a -> Bit 1)
    -> (a -> Bit (SizeWidth p))
    -> Int
    -> Sink a
    -> Module [Sink a]
makeSharedBurstSink hasData getSize n sink = do
  liftNat (log2 n) $ \ (_ :: Proxy width) -> do
    let laneSize = constant $ toInteger $ valueOf @(LaneWidth p)
    token :: Reg (Bit width) <- makeReg dontCare
    sinks <- makeSharedSink n sink
    size :: Reg TLSize <- makeReg 0

    return
      [
        Sink
          { canPut= s.canPut .&&. (size.val === 0 .||. token.val === constant i)
          , put= \ msg -> do
              token <== constant i
              let sz :: TLSize = size.val === 0 ? (1 .<<. getSize msg, size.val)
              size <== (sz .<=. laneSize .||. inv (hasData msg)) ?
                (0, sz - laneSize)
              s.put msg }
      | (s, i) <- zip sinks [0..] ]

makeSharedSinkA :: forall p. KnownTLParams p => Int -> Sink (ChannelA p) -> Module [Sink (ChannelA p)]
makeSharedSinkA i = makeSharedBurstSink @p (\ msg -> hasDataA msg.opcode) (\ msg -> msg.size) i

makeSharedSinkB :: forall p. KnownTLParams p => Int -> Sink (ChannelB p) -> Module [Sink (ChannelB p)]
makeSharedSinkB i = makeSharedSink i

makeSharedSinkC :: forall p. KnownTLParams p => Int -> Sink (ChannelC p) -> Module [Sink (ChannelC p)]
makeSharedSinkC i = makeSharedBurstSink @p (\ msg -> hasDataC msg.opcode) (\ msg -> msg.size) i

makeSharedSinkD :: forall p. KnownTLParams p => Int -> Sink (ChannelD p) -> Module [Sink (ChannelD p)]
makeSharedSinkD i = makeSharedBurstSink @p (\ msg -> hasDataD msg.opcode) (\ msg -> msg.size) i

makeSharedSinkE :: forall p. KnownTLParams p => Int -> Sink (ChannelE p) -> Module [Sink (ChannelE p)]
makeSharedSinkE i = makeSharedSink i

data XBarConfig n m p =
  XBarConfig
    { bce :: Bool
    , rootSource :: Bit (SourceWidth p) -> Bit (Log2 m)
    , rootSink :: Bit (SinkWidth p) -> Bit (Log2 n)
    , rootAddr :: Bit (AddrWidth p) -> Bit (Log2 m)
    , sizeChannelA :: Int
    , sizeChannelB :: Int
    , sizeChannelC :: Int
    , sizeChannelD :: Int
    , sizeChannelE :: Int }

makeTLXBarNoBCE :: forall n m p.
  (KnownNat n, KnownNat m, KnownTLParams p, KnownNat (Log2 n), KnownNat (Log2 m))
    => XBarConfig n m p
    -> Module ([TLMaster p], [TLSlave p])
makeTLXBarNoBCE config = do
  queueA :: Queue (ChannelA p) <- buildQueue config.sizeChannelA
  queueD :: Queue (ChannelD p) <- buildQueue config.sizeChannelD

  let laneSize :: TLSize = constant (toInteger (valueOf @(LaneWidth p)))

  tokenA :: Reg (Bit (Log2 m)) <- makeReg dontCare
  tokenD :: Reg (Bit (Log2 m)) <- makeReg dontCare
  sizeA :: Reg TLSize <- makeReg 0
  sizeD :: Reg TLSize <- makeReg 0

  sinkA <- makeSharedSinkA @p (valueOf @m) (toSink queueA)
  sinkD <- makeSharedSinkD @p (valueOf @n) (toSink queueD)
  let sourceA = toSource queueA
  let sourceD = toSource queueD

  let rootA =
        [constant i === config.rootAddr sourceA.peek.address
          | i <- [0..toInteger $ valueOf @n - 1]]
  let rootD =
        [constant i === config.rootSource sourceD.peek.source
          | i <- [0..toInteger $ valueOf @m - 1]]

  let canPeekA = map (sourceA.canPeek .&&.) rootA
  let canPeekD = map (sourceD.canPeek .&&.) rootD

  return ([
      TLMaster
        { channelA= sourceA{canPeek=canPeekA!i}
        , channelB= nullSink
        , channelC= nullSource
        , channelD= sinkD!i
        , channelE= nullSource}

    | i <- [0..toInteger $ valueOf @n - 1]],[
      TLSlave
        { channelA= sinkA!i
        , channelB= nullSource
        , channelC= nullSink
        , channelD= sourceD{canPeek= canPeekD!i}
        , channelE= nullSink}

    | i <- [0..toInteger $ valueOf @m - 1]])
  where
    buildQueue 0 = makeBypassQueue
    buildQueue 1 = makePipelineQueue 1
    buildQueue 2 = makeQueue
    buildQueue n = makeSizedQueueCore n

-- n is the number of masters, and m the number of slaves
makeTLXBarBCE :: forall n m p.
  (KnownNat n, KnownNat m, KnownTLParams p, KnownNat (Log2 n), KnownNat (Log2 m))
    => XBarConfig n m p
    -> Module ([TLMaster p], [TLSlave p])
makeTLXBarBCE config = do
  queueA :: Queue (ChannelA p) <- buildQueue config.sizeChannelA
  queueB :: Queue (ChannelB p) <- buildQueue config.sizeChannelB
  queueC :: Queue (ChannelC p) <- buildQueue config.sizeChannelC
  queueD :: Queue (ChannelD p) <- buildQueue config.sizeChannelD
  queueE :: Queue (ChannelE p) <- buildQueue config.sizeChannelE

  let laneSize :: TLSize = constant (toInteger (valueOf @(LaneWidth p)))

  tokenA :: Reg (Bit (Log2 m)) <- makeReg dontCare
  tokenC :: Reg (Bit (Log2 m)) <- makeReg dontCare
  tokenD :: Reg (Bit (Log2 m)) <- makeReg dontCare
  sizeA :: Reg TLSize <- makeReg 0
  sizeC :: Reg TLSize <- makeReg 0
  sizeD :: Reg TLSize <- makeReg 0

  sinkA <- makeSharedSinkA @p (valueOf @m) (toSink queueA)
  sinkB <- makeSharedSinkB @p (valueOf @n) (toSink queueB)
  sinkC <- makeSharedSinkC @p (valueOf @m) (toSink queueC)
  sinkD <- makeSharedSinkD @p (valueOf @n) (toSink queueD)
  sinkE <- makeSharedSinkE @p (valueOf @m) (toSink queueE)
  let sourceA = toSource queueA
  let sourceB = toSource queueB
  let sourceC = toSource queueC
  let sourceD = toSource queueD
  let sourceE = toSource queueE

  let rootA =
        [constant i === config.rootAddr sourceA.peek.address
          | i <- [0..toInteger $ valueOf @n - 1]]
  let rootB =
        [constant i === config.rootSource sourceB.peek.source
          | i <- [0..toInteger $ valueOf @m - 1]]
  let rootC =
        [constant i === config.rootAddr sourceC.peek.address
          | i <- [0..toInteger $ valueOf @n - 1]]
  let rootD =
        [constant i === config.rootSource sourceD.peek.source
          | i <- [0..toInteger $ valueOf @m - 1]]
  let rootE =
        [constant i === config.rootSink sourceE.peek.sink
          | i <- [0..toInteger $ valueOf @n - 1]]

  let canPeekA = map (sourceA.canPeek .&&.) rootA
  let canPeekB = map (sourceB.canPeek .&&.) rootB
  let canPeekC = map (sourceC.canPeek .&&.) rootC
  let canPeekD = map (sourceD.canPeek .&&.) rootD
  let canPeekE = map (sourceE.canPeek .&&.) rootE

  return ([
      TLMaster
        { channelA= sourceA{canPeek=canPeekA!i}
        , channelB= sinkB!i
        , channelC= sourceC{canPeek=canPeekC!i}
        , channelD= sinkD!i
        , channelE= sourceE{canPeek=canPeekE!i}}

    | i <- [0..toInteger $ valueOf @n - 1]],[
      TLSlave
        { channelA= sinkA!i
        , channelB= sourceB{canPeek=canPeekB!i}
        , channelC= sinkC!i
        , channelD= sourceD{canPeek= canPeekD!i}
        , channelE= sinkE!i}

    | i <- [0..toInteger $ valueOf @m - 1]])
  where
    buildQueue 0 = makeBypassQueue
    buildQueue 1 = makePipelineQueue 1
    buildQueue 2 = makeQueue
    buildQueue n = makeSizedQueueCore n

makeTLXBar :: forall n m p.
  (KnownNat n, KnownNat m, KnownTLParams p, KnownNat (Log2 n), KnownNat (Log2 m))
    => XBarConfig n m p
    -> Module ([TLMaster p], [TLSlave p])
makeTLXBar config =
  if config.bce then
    makeTLXBarBCE @n @m @p config
  else
    makeTLXBarNoBCE @n @m @p config
