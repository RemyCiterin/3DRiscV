module TileLink.Interconnect where

import Blarney
import Blarney.Connectable
import Blarney.SourceSink
import Blarney.Sharing
import Blarney.Queue

import TileLink.Types
import TileLink.Utils

import Data.Proxy

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

-- n is the number of masters, and m the number of slaves
makeTLXBar :: forall n m p.
  (KnownNat n, KnownNat m, KnownTLParams p, KnownNat (Log2 n), KnownNat (Log2 m))
    => XBarConfig n m p
    -> Module ([TLMaster p], [TLSlave p])
makeTLXBar config = do
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

  sinkA <- makeSharedSink (valueOf @m) (toSink queueA)
  sinkB <- makeSharedSink (valueOf @n) (toSink queueB)
  sinkC <- makeSharedSink (valueOf @m) (toSink queueC)
  sinkD <- makeSharedSink (valueOf @n) (toSink queueD)
  sinkE <- makeSharedSink (valueOf @m) (toSink queueE)
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
        [constant i === config.rootSink sourceD.peek.sink
          | i <- [0..toInteger $ valueOf @n - 1]]

  let canPeekA = map (sourceA.canPeek .&&.) rootA
  let canPeekB = map (sourceB.canPeek .&&.) rootB
  let canPeekC = map (sourceC.canPeek .&&.) rootC
  let canPeekD = map (sourceD.canPeek .&&.) rootD
  let canPeekE = map (sourceE.canPeek .&&.) rootE

  let canPutD =
        [s.canPut .&&. (sizeD.val === 0 .||. constant i === tokenD.val)
          | (s,i) <- zip sinkD [0..]]


  let canPutA =
        [s.canPut .&&. (sizeA.val === 0 .||. constant i === tokenA.val)
          | (s,i) <- zip sinkA [0..]]


  let canPutC =
        [s.canPut .&&. (sizeC.val === 0 .||. constant i === tokenC.val)
          | (s,i) <- zip sinkC [0..]]

  return ([
      TLMaster
        { channelA= sourceA{canPeek=canPeekA!i}
        , channelB= sinkB!i
        , channelC= sourceC{canPeek=canPeekC!i}
        , channelD=
            Sink
              { canPut= canPutD!i
              , put= \ x -> do
                  (sinkD!i).put x
                  tokenD <== constant i
                  let size :: TLSize = sizeD.val === 0 ? (1 .<<. x.size, sizeD.val)
                  sizeD <== (size .<=. laneSize .||. inv (hasDataD x.opcode)) ?
                    (0, size - laneSize)}
        , channelE= sourceE{canPeek=canPeekE!i}}

    | i <- [0..toInteger $ valueOf @n - 1]],[
      TLSlave
        { channelA=
            Sink
              { canPut= canPutA!i
              , put= \ x -> do
                  (sinkA!i).put x
                  tokenA <== constant i
                  let size :: TLSize = sizeA.val === 0 ? (1 .<<. x.size, sizeA.val)
                  sizeA <== (size .<=. laneSize .||. inv (hasDataA x.opcode)) ?
                    (0, size - laneSize)}
        , channelB= sourceB{canPeek=canPeekB!i}
        , channelC=
            Sink
              { canPut= canPutC!i
              , put= \ x -> do
                  (sinkC!i).put x
                  tokenC <== constant i
                  let size :: TLSize = sizeC.val === 0 ? (1 .<<. x.size, sizeC.val)
                  sizeC <== (size .<=. laneSize .||. inv (hasDataC x.opcode)) ?
                    (0, size - laneSize)}
        , channelD= sourceD{canPeek= canPeekD!i}
        , channelE= sinkE!i}

    | i <- [0..toInteger $ valueOf @m - 1]])
  where
    buildQueue 0 = makeBypassQueue
    buildQueue 2 = makeQueue
    buildQueue 1 = makePipelineQueue 1
    buildQueue n = makeSizedQueueCore n
