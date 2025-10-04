module Gpu where

import Blarney
import Blarney.Ehr
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.TypeFamilies
import Blarney.Connectable
import Blarney.Utils
import Blarney.Arbiter
import Blarney.Vector (Vec, fromList, toList)
import qualified Blarney.Vector as Vec
import Blarney.QuadPortRAM
import Blarney.Stmt
import Blarney.ADT

import MulDiv

import System
import Instr
import Clint
import Uart
import Alu
import CSR

import TileLink
import TileLink.CoherentBCache
import TileLink.Broadcast
import TileLink.BusWidth

debug :: Bool
debug = False

displayAscii :: Bit 8 -> Action ()
displayAscii term =
  display_ $ go term $ 10 : 13 : [32..126]
  where
    ascii :: [String] = [[toEnum i] | i <- [0..255]]

    go :: Bit 8 -> [Integer] -> Format
    go x [] = formatCond false (fshow "")
    go x (i : is)  =
      formatCond (fromInteger i === x) (fshow (ascii!i)) <>
      go x is

-- Select -> Fetch -> Decode -> Register Read -> Exec -> Write Back
--    ^                                                     /
--    \____________________________________________________/
--
--
-- 16 warps of 4 threads

type LogNumWarp = 4
type WarpId = Bit LogNumWarp

type WarpLogSize = 2
type WarpSize = 2 ^ WarpLogSize
type WarpMask = Bit WarpSize
type WarpIdx = Bit (Log2Ceil WarpSize)

type LogNumLevel = 5
type Level = Bit LogNumLevel

data Select2Fetch =
  Select2Fetch
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , pc :: Bit 32 }
  deriving(Generic, Bits)

data Fetch2Decode =
  Fetch2Decode
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , pc :: Bit 32
    , instr :: Bit 32 }
  deriving(Generic, Bits)

data Decode2RR =
  Decode2RR
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , pc :: Bit 32
    , instr :: Instr }
  deriving(Generic, Bits)

data RR2Exec =
  RR2Exec
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , pc :: Bit 32
    , instr :: Instr
    , op1 :: Vec WarpSize (Bit 32)
    , op2 :: Vec WarpSize (Bit 32) }
  deriving(Generic, Bits)

data Exec2WB =
  Exec2WB
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , instr :: Instr
    , rd :: Vec WarpSize (Bit 32)
    , nextPc :: Vec WarpSize (Bit 32)
    , pc :: Bit 32 }
  deriving(Generic, Bits)

data WB2Select =
  WB2Select
    { warp :: WarpId
    , mask :: WarpMask
    , level :: Level
    , pc :: Vec WarpSize (Bit 32) }
  deriving(Generic, Bits)

type WriteBack = WarpId -> WarpMask -> RegId -> Vec WarpSize (Bit 32) -> Action ()

makeFetch :: forall p.
  ( KnownTLParams p
  , AddrWidth p ~ 32
  , LaneWidth p ~ 4
  , 8 * LaneWidth p ~ 32 )
    => Bit (SourceWidth p) -> Source Select2Fetch -> Module (Source Fetch2Decode, TLMaster p)
makeFetch cacheSource inputs = do
  (cache, master) <- withName "icache" $
    makeBCacheCore @2 @20 @6 @4 @() @p cacheSource (\ _ lane -> lane)
  key :: Reg (Bit 20) <- makeReg dontCare

  responses :: Queue (Bit 32) <- makeSizedQueueCore 4
  queue :: Queue Select2Fetch <- makePipelineQueue 3

  always do
    when (cache.canLookup .&&. inputs.canPeek .&&. queue.notFull) do
      let (msb_index,offset) = split (slice @31 @2 inputs.peek.pc)
      let (msb, index) = split msb_index
      cache.lookup index offset (item #Load)
      queue.enq inputs.peek
      inputs.consume
      key <== msb

    when (cache.canMatch) do
      cache.match key.val

    when (cache.loadResponse.canPeek .&&. responses.notFull) do
      responses.enq cache.loadResponse.peek
      cache.loadResponse.consume

  return
    ( Source
        { canPeek= queue.canDeq .&&. responses.canDeq
        , consume= do
            responses.deq
            queue.deq
        , peek=
            Fetch2Decode
              { warp= queue.first.warp
              , mask= queue.first.mask
              , level= queue.first.level
              , pc= queue.first.pc
              , instr= responses.first }}
    , master)

makeDecode :: Source Fetch2Decode -> Module (Source Decode2RR)
makeDecode inputs = do
  queue :: Queue Decode2RR <- makePipelineQueue 1

  always do
    when (queue.notFull .&&. inputs.canPeek) do
      inputs.consume
      queue.enq
        Decode2RR
          { warp= inputs.peek.warp
          , mask= inputs.peek.mask
          , level= inputs.peek.level
          , pc= inputs.peek.pc
          , instr= decodeInstrGpu inputs.peek.instr }
  return (toSource queue)

makeHalfRegisterFile :: Source Decode2RR -> Module (Source RR2Exec, WriteBack)
makeHalfRegisterFile inputs = do
  rf :: [RAM (Bit (5 + LogNumWarp - 1)) (Bit 32)] <-
    Blarney.replicateM (valueOf @WarpSize) makeDualRAMForward

  let result :: Vec WarpSize (Bit 32) = fromList [r.out | r <- rf]
  stage1 :: Queue Decode2RR <- makePipelineQueue 1
  stage2 :: Queue (Vec WarpSize (Bit 32)) <- makePipelineQueue 1
  queue :: Queue () <- makePipelineQueue 1

  always do
    when (stage1.notFull .&&. inputs.canPeek .&&. queue.notFull) do
      let msb :: Bit (LogNumWarp-1) = truncateLSB inputs.peek.warp
      forM_ [0..valueOf @WarpSize - 1] \ i -> do
        (rf!i).load (inputs.peek.instr.rs1.val # msb)

      stage1.enq inputs.peek
      inputs.consume
      queue.enq ()

    when (stage2.notFull .&&. stage1.canDeq .&&. queue.canDeq) do
      let msb :: Bit (LogNumWarp-1) = truncateLSB stage1.first.warp
      forM_ [0..valueOf @WarpSize - 1] \ i -> do
        (rf!i).load (stage1.first.instr.rs2.val # msb)

      stage2.enq result
      queue.deq

  return
    ( Source
        { canPeek= stage1.canDeq .&&. stage2.canDeq
        , consume= do
            stage1.deq
            stage2.deq
        , peek=
            RR2Exec
              { warp= stage1.first.warp
              , mask= stage1.first.mask
              , level= stage1.first.level
              , pc= stage1.first.pc
              , instr= stage1.first.instr
              , op1= stage2.first
              , op2= result }}
    , \ id mask register values -> do
        forM_ [0..valueOf @WarpSize - 1] \ i -> do
          when (mask!i .&&. register =!= 0) do
            (rf!i).store (register # truncateLSB id) (values!i)
    )

-- Define a two stages register file using general SRAM.
-- To improve the performances it's better to send request
-- with alternate parity of warp id's.
makeRegisterRead :: Source Decode2RR -> Module (Source RR2Exec, WriteBack)
makeRegisterRead inputs = do
  (sourceE, wbE) <-
        makeHalfRegisterFile
          Source
            { peek= inputs.peek
            , consume= inputs.consume
            , canPeek= inputs.canPeek .&&. at @0 inputs.peek.warp === 0 }

  (sourceO, wbO) <-
        makeHalfRegisterFile
          Source
            { peek= inputs.peek
            , consume= inputs.consume
            , canPeek= inputs.canPeek .&&. at @0 inputs.peek.warp === 1 }

  queue :: Queue RR2Exec <- makePipelineQueue 1

  let source =
        Source
          { canPeek= sourceE.canPeek .||. sourceO.canPeek
          , consume= do
              if sourceE.canPeek
              then sourceE.consume
              else sourceO.consume
          , peek= sourceE.canPeek ? (sourceE.peek, sourceO.peek) }

  makeConnection source (toSink queue)

  return
    ( toSource queue
    , \ id mask register values -> do
        if at @0 id === 0
        then wbE id mask register values
        else wbO id mask register values
    )

data SimtState =
  SimtState
    { busy :: Bit 1
    , pc :: Bit 32
    , level :: Level }
  deriving(Generic, Bits)

makeSelect :: Source WB2Select -> Module (Source Select2Fetch)
makeSelect inputs = do
  (statesA, statesB) :: ([RAM WarpId SimtState], [RAM WarpId SimtState]) <-
    unzip <$> replicateM (valueOf @WarpSize) makeQuadRAM

  -- Use a round robin algorithm to choose the current program counter of the warp
  round :: Reg WarpIdx <- makeReg 0

  warp :: Ehr WarpId <- makeEhr 2 0
  pc :: Wire (Bit 32) <- makeWire dontCare

  -- Buffer the outputs of the select stage
  queue :: Queue Select2Fetch <-makePipelineQueue 1

  initIndex :: Reg WarpId <- makeReg 0
  initDone :: Reg (Bit 1) <- makeReg false

  let level :: Level = tree1 (\ x y -> x .>. y ? (x,y)) [s.out.level | s <- statesA]

  always do
    -- Write back results from exec stage
    when inputs.canPeek do
      forM_ [0..valueOf @WarpSize - 1] \ i -> do
        when (inputs.peek.mask!i) do
          (statesB!i).store inputs.peek.warp
            SimtState
              { busy= false
              , pc= inputs.peek.pc!i
              , level= inputs.peek.level }
      inputs.consume

    sequence_ [s.load (warp.read 1) | s <- statesA]

    -- Scheduler: choose the first thread availables from `round.val`
    let choosen :: WarpMask =
          let actives = fromBitList [inv s.out.busy .&&. s.out.level === level | s <- statesA] in
          rotr (firstHot (rotl actives round.val)) round.val

    forM_ [0..valueOf @WarpSize-1] \ i -> do
      when (choosen!i) do pc <== (statesA!i).out.pc

    when (inv initDone.val) do
      sequence_
        [ s.store initIndex.val SimtState{busy= false, pc= 0x80000000, level= 1}
        | s <-statesA ]
      initDone <== initIndex.val + 1 === 0
      initIndex <== initIndex.val + 1

  let mask =
        fromBitList
          [inv s.out.busy .&&. s.out.pc === pc.val .&&. s.out.level === level
            | s <- statesA]

  let source :: Source Select2Fetch =
        Source
          { consume= do
              sequence_
                [ when (unsafeAt i mask) do
                  s.store (warp.read 0) SimtState{busy= true, pc= s.out.pc, level= level}
                | (s,i) <- zip statesA [0..]]
              warp.write 0 (1 + warp.read 0)
              when (warp.read 0 === 0) do
                round <== round.val + 1
          , peek=
              Select2Fetch
                { mask
                , level
                , warp= warp.read 0
                , pc= pc.val }
          , canPeek=
              orList [inv s.out.busy | s <- statesA] .&&.
              inv (inputs.canPeek .&&. warp.read 0 === inputs.peek.warp) .&&.
              initDone.val }

  makeConnection source (toSink queue)
  return (toSource queue)

data CoalescedMemop =
  CoalescedMemop
    { width :: Bit 2
    -- ^ width of the memory access
    , isUnsigned :: Bit 1
    -- ^ in case of a load with a size less than a word, do we use signed or zero extension
    , isStore :: Bit 1
    -- ^ is the current request a load or a store
    , level :: Level
    -- ^ level in the virtual threads stack
    , mask :: WarpMask
    -- ^ execution mask of the warp
    , warp :: WarpId
    -- ^ ID of the warp
    , instr :: Instr
    -- ^ decoded instruction
    , pc :: Bit 32
    -- ^ program counter of the memory instruction
    , offset :: Vec WarpSize (Bit (2 + WarpLogSize))
    -- ^ for each thread, offset of it's data into the memory block (in bytes)
    , block :: Bit (32 - 2 - WarpLogSize)
    -- ^ base address of the memory block
    , lane :: Bit (32 * WarpSize)
    -- ^ data operands of the warp
    , strb :: Bit (4 * WarpSize)
    -- ^ byte enabled of the warp
    } deriving(Bits, Generic)

-- I the address is greater than 0x80000000, then the access is cached ane multiple accesses can be
-- coalesced into one, otherwise accesses must be done sequentially
isCached :: Bit 32 -> Bit 1
isCached addr = addr .>=. 0x80000000

-- Transform parallel memory requests using arbitrary addresses into a stream of
-- structured memory requests using a common block of size (4 * WarpSize) bytes.
-- This is a key performance optimisation in GPUs because it allow multiple
-- memory requests to be transformed in one bigger requests if their addresses
-- are close enough.
makeCoalescingUnit :: Source RR2Exec -> Module (Source CoalescedMemop)
makeCoalescingUnit inputs = do
  req0 :: Reg RR2Exec <- makeReg dontCare
  valid0 :: Ehr (Bit 1) <- makeEhr 2 false

  width1 :: Reg (Bit 2) <- makeReg dontCare
  level1 :: Reg Level <- makeReg dontCare
  isUnsigned1 :: Reg (Bit 1) <- makeReg dontCare
  isStore1 :: Reg (Bit 1) <- makeReg dontCare
  mask1 :: Reg WarpMask <- makeReg dontCare
  warp1 :: Reg WarpId <- makeReg dontCare
  instr1 :: Reg Instr <- makeReg dontCare
  pc1 :: Reg (Bit 32) <- makeReg dontCare
  offset1 :: Reg (Vec WarpSize (Bit (2 + WarpLogSize))) <- makeReg dontCare
  block1 :: Reg (Bit (32 - 2 - WarpLogSize)) <- makeReg dontCare
  lane1 :: Reg (Vec WarpSize (Bit 32)) <- makeReg dontCare
  valid1 :: Ehr (Bit 1) <- makeEhr 2 false

  always do
    when (inv (valid0.read 1) .&&. inputs.canPeek) do
      req0 <== inputs.peek
      valid0.write 1 true
      inputs.consume

    when (valid0.read 0 .&&. inv (valid1.read 1)) do
      let mask = req0.val.mask
      let instr = req0.val.instr
      let addresses = [instr.imm.val + op1 | op1 <- toList req0.val.op1]
      let leader = firstHot mask
      let base =
            select
              [ cond --> slice @31 @(WarpLogSize+2) addr
                | (cond, addr) <- zip (toBitList leader) addresses]
      let actives :: WarpMask =
            if isCached (base # 0)
            then
              fromBitList
                [slice @31 @(WarpLogSize+2) a === base .&&. unsafeAt i mask
                  | (a,i) <- zip addresses [0..]]
            else
              leader

      -- Set stage 1 to busy
      valid1.write 1 true

      -- if the coalescing succede, free stage 0, otherwise free the activated lanes
      if actives === mask then do
        valid0.write 0 false
      else
        req0 <== (req0.val{mask= mask .&. inv actives}::RR2Exec)

      -- Write stage 1 state
      width1 <== instr.accessWidth
      isUnsigned1 <== instr.isUnsigned
      isStore1 <== instr.opcode `is` [STORE]
      mask1 <== actives
      level1 <== req0.val.level
      warp1 <== req0.val.warp
      instr1 <== instr
      pc1 <== req0.val.pc
      offset1 <== fromList [slice @(WarpLogSize+1) @0 a | a <- addresses]
      block1 <== base
      lane1 <== req0.val.op2

  let genStrb :: Bit 1 -> Bit 2 -> Bit (4 * WarpSize) = \ active width ->
        select
          [ width === 0b00 .&&. active --> 0b0001
          , width === 0b01 .&&. active --> 0b0011
          , width === 0b10 .&&. active --> 0b1111 ]

  let strb =
        orList
          [ (genStrb active width1.val) .<<. off
            | (off, active) <- zip (toList offset1.val) (toBitList mask1.val)]

  -- Sequential merging of the store requests of all the threads
  let lane =
        foldr
          (\ (word, off, active) acc ->
            let new = (zeroExtend word) .<<. (off # (0::Bit 3)) in
            mergeBE new acc (genStrb active width1.val .<<. off)
          ) 0 (zip3 (toList lane1.val) (toList offset1.val) (toBitList mask1.val))

  return
    Source
      { canPeek= valid1.read 0
      , consume= valid1.write 0 false
      , peek=
          CoalescedMemop
            { width= width1.val
            , isUnsigned= isUnsigned1.val
            , isStore= isStore1.val
            , mask= mask1.val
            , warp= warp1.val
            , level= level1.val
            , instr= instr1.val
            , pc= pc1.val
            , offset= offset1.val
            , block= block1.val
            , lane
            , strb }}

makeDCacheUnit :: forall p.
  ( KnownTLParams p
  , AddrWidth p ~ 32
  , LaneWidth p ~ 4 * WarpSize
  , 8 * LaneWidth p ~ 32 * WarpSize )
    => Bit (SourceWidth p) -> Source RR2Exec -> Module (Source Exec2WB, TLMaster p, BCacheStats)
makeDCacheUnit cacheSource inputs0 = do
  inputs <- makeCoalescingUnit inputs0

  (cache, master) <- withName "dcache" $
    makeBCacheCore @2 @20 @6 @(4-WarpLogSize) @() @p cacheSource (\ _ lane -> lane)
  key :: Reg (Bit 20) <- makeReg dontCare

  queue :: Queue CoalescedMemop <- makePipelineQueue 2

  always do
    when (inputs.canPeek .&&. queue.notFull .&&. cache.canLookup) do
      let addr :: Bit 32 = inputs.peek.block # 0
      let (msb_index, offset) = split inputs.peek.block
      let (msb, index) = split msb_index
      key <== msb

      when (addr === 0x10000000 .&&. inputs.peek.isStore .&&. at @0 inputs.peek.strb) do
        when debug (display_ "print char: ")
        displayAscii (slice @7 @0 (inputs.peek.lane))
        when debug (display "")

      if inputs.peek.isStore
      then do
        when debug do
          display
            "        [0x"
            (formatHex 0 addr)
            "] <= "
            (formatHex 0 inputs.peek.lane)
            " if 0x"
            (formatHex 1 inputs.peek.strb)
        cache.lookup index offset (tag #Store (inputs.peek.strb, inputs.peek.lane))
      else do
        when debug do
          display "        read at addr: " (formatHex 0 addr)
        cache.lookup index offset (item #Load)

      queue.enq inputs.peek
      inputs.consume

    when (cache.canMatch) do
      cache.match key.val

  let isByte :: Bit 2 -> Bit 1 = \ width -> width === 0b00
  let isHalf :: Bit 2 -> Bit 1 = \ width -> width === 0b01
  let isWord :: Bit 2 -> Bit 1 = \ width -> width === 0b10

  let genRd i =
        let width = queue.first.width in
        let unsigned = queue.first.isUnsigned in
        let bytes = cache.loadResponse.peek .>>. ((queue.first.offset!i) # (0 :: Bit 3)) in
        select
          [ isHalf width .&&. inv unsigned --> signExtend (slice @15 @0 bytes)
          , isByte width .&&. inv unsigned --> signExtend (slice @7 @0 bytes)
          , isHalf width .&&. unsigned --> zeroExtend (slice @15 @0 bytes)
          , isByte width .&&. unsigned --> zeroExtend (slice @7 @0 bytes)
          , isWord width --> slice @31 @0 bytes ]

  let lanes :: [Bit 32] = [ genRd i | i <- [0..valueOf @WarpSize - 1]]

  return
    ( Source
      { canPeek= queue.canDeq .&&. (queue.first.isStore .||. cache.loadResponse.canPeek)
      , consume= do
        when (inv queue.first.isStore) do
          cache.loadResponse.consume
        queue.deq
      , peek=
        Exec2WB
          { warp= queue.first.warp
          , mask= queue.first.mask
          , level= queue.first.level
          , instr= queue.first.instr
          , pc= queue.first.pc
          , nextPc= fromList (replicate (valueOf @WarpSize) (queue.first.pc + 4))
          , rd= fromList lanes}}
    , master
    , cache.stats )

makeAluMultiplier :: Source RR2Exec -> Module (Source Exec2WB)
makeAluMultiplier inputs = do
  queue :: Queue Exec2WB <- makePipelineQueue 1

  always do
    when (inputs.canPeek .&&. queue.notFull) do
      inputs.consume
      queue.enq
        Exec2WB
          { pc= inputs.peek.pc
          , warp= inputs.peek.warp
          , level= inputs.peek.level
          , mask= inputs.peek.mask
          , instr= inputs.peek.instr
          , nextPc= fromList (replicate (valueOf @WarpSize) (inputs.peek.pc + 4))
          , rd= mulUpper ? (fmap upper mulOut, fmap lower mulOut)}

  return (toSource queue)
  where
    op1 = inputs.peek.op1
    op2 = inputs.peek.op2
    instr = inputs.peek.instr
    opcode = instr.opcode

    mulLhs, mulRhs, mulOut :: Vec WarpSize (Bit 64)
    mulLhs = opcode `is` [MUL,MULH,MULHSU] ? (fmap signExtend op1, fmap zeroExtend op1)
    mulRhs = opcode `is` [MUL,MULH] ? (fmap signExtend op2, fmap zeroExtend op2)
    mulOut = fromList (fmap (\ (x,y) -> x*y) (zip (toList mulLhs) (toList mulRhs)))
    mulUpper = opcode `is` [MULH, MULHSU, MULHU]

makeAluDivider :: Source RR2Exec -> Module (Source Exec2WB)
makeAluDivider inputs = do
  dividers :: [Server (Bit 34, Bit 34) (Bit 34, Bit 34)] <-
    replicateM (valueOf @WarpSize) makeDivider

  idle :: Reg (Bit 1) <- makeReg true

  always do
    when (idle.val .&&. inputs.canPeek .&&. andList [d.reqs.canPut | d <- dividers]) do
      let mask = inputs.peek.mask
      sequence
        [ when (unsafeAt i mask) do
            d.reqs.put (divNum!i, divDen!i)
        | (d,i) <- zip dividers [0..] ]
      idle <== false

  let mask = inputs.peek.mask
  let rd :: Vec WarpSize (Bit 32) =
        fmap (\ (x, y, out) ->
          select
            [ y === 0 --> isRem ? (0, -1)
            , divOverflow x y --> isRem ? (0,x)
            , inv (divOverflow x y) .&&. y =!= 0 -->
                isRem ? (lower out.snd, lower out.fst) ]
        ) (Vec.zip3 op1 op2 (fromList [d.resps.peek | d <- dividers]))
  return
    Source
      { canPeek=
          inv idle.val .&&.
          inputs.canPeek .&&.
          mask === fromBitList [d.resps.canPeek | d <- dividers]
      , consume= do
          inputs.consume
          idle <== true
          sequence_
            [ when cond d.resps.consume
            | (cond,d) <- zip (toBitList mask) dividers ]
      , peek=
        Exec2WB
          { pc= inputs.peek.pc
          , warp= inputs.peek.warp
          , level= inputs.peek.level
          , mask= inputs.peek.mask
          , instr= inputs.peek.instr
          , nextPc= fromList (replicate (valueOf @WarpSize) (inputs.peek.pc + 4))
          , rd } }
  where
    op1 = inputs.peek.op1
    op2 = inputs.peek.op2
    instr = inputs.peek.instr
    opcode = instr.opcode

    divNum, divDen :: Vec WarpSize (Bit 34)
    divNum = opcode `is` [DIV,REM] ? (fmap signExtend op1, fmap zeroExtend op1)
    divDen = opcode `is` [DIV,REM] ? (fmap signExtend op2, fmap zeroExtend op2)
    isRem = opcode `is` [REM,REMU]

    divOverflow :: Bit 32 -> Bit 32 -> Bit 1
    divOverflow x y =
          opcode `is` [DIV,REM] .&&. signedDivOverflow (x,y)

makeExec :: forall p.
  ( KnownTLParams p
  , AddrWidth p ~ 32
  , LaneWidth p ~ 4 * WarpSize
  , 8 * LaneWidth p ~ 32 * WarpSize )
    => Bit 32
    -> Bit (SourceWidth p)
    -> Source RR2Exec
    -> Module (Source Exec2WB, TLMaster p)
makeExec instret cacheSource inputs = do
  (dmemOut, master, stats) <-
    makeDCacheUnit @p cacheSource
      (inputs{canPeek= inputs.canPeek .&&. inputs.peek.instr.isMemAccess})

  let isDivRem = inputs.peek.instr.opcode `is` [DIV,DIVU,REM,REMU]
  let isMul = inputs.peek.instr.opcode `is` [MUL,MULH,MULHSU,MULHU]

  divider <- makeAluDivider inputs{canPeek= inputs.canPeek .&&. isDivRem}
  multiplier <- makeAluMultiplier inputs{canPeek= inputs.canPeek .&&. isMul}

  -- arbiters for the write-back stage queue between the memory/alu responses
  [arbiter1, arbiter2, arbiter3, arbiter4] <- makeFairArbiter 4

  -- return the program counters to toe scheduler
  outputs :: Queue Exec2WB <- makeQueue

  nextPC :: Wire (Vec WarpSize (Bit 32)) <- makeWire dontCare
  nextRD :: Wire (Vec WarpSize (Bit 32)) <- makeWire dontCare
  level :: Wire Level <- makeWire inputs.peek.level

  cycle :: Reg (Bit 32) <- makeReg 0
  always $ cycle <== cycle.val + 1

  --always do
  --  when (slice @20 @0 cycle.val === 0) do
  --    display "stats: "
  --    display "\thit: " stats.numHit
  --    display "\treq: " stats.numReq
  --    display "\tacquire: " stats.numAcquire
  --    display "\trelease: " stats.numRelease
  --    display "\tprobe: " stats.numProbe

  always do
    when (outputs.notFull .&&. dmemOut.canPeek) do
      arbiter1.request
    when (arbiter1.grant) do
      outputs.enq dmemOut.peek
      dmemOut.consume

    when (outputs.notFull .&&. multiplier.canPeek) do
      arbiter3.request
    when (arbiter3.grant) do
      outputs.enq multiplier.peek
      multiplier.consume

    when (outputs.notFull .&&. divider.canPeek) do
      arbiter4.request
    when (arbiter4.grant) do
      outputs.enq divider.peek
      divider.consume

    let isAlu = inv inputs.peek.instr.isMemAccess .&&. inv isMul .&&. inv isDivRem

    when (inputs.canPeek .&&. outputs.notFull .&&. isAlu) do
      let op1 = toList inputs.peek.op1
      let op2 = toList inputs.peek.op2
      let instr = inputs.peek.instr
      let mask = inputs.peek.mask
      let warp = inputs.peek.warp
      let pc = inputs.peek.pc

      let results =
            [ alu
                ExecInput
                  { pc= pc
                  , instr= instr
                  , rs1
                  , rs2 }
            | (rs1,rs2) <- zip op1 op2 ]

      -- read thread id
      if instr.opcode `is` [CSRRW,CSRRS,CSRRC] .&&. instr.csr === 0xf14 then do
        nextRD <== fromList
              [ 0 # warp # (lit (toInteger i) :: WarpIdx) | i <- [0..valueOf @WarpSize - 1]]
      else if instr.opcode `is` [CSRRW,CSRRS,CSRRC] .&&. instr.csr === 0xb00 then do
        nextRD <== fromList
              [ cycle.val | i <- [0..valueOf @WarpSize - 1]]
      else if instr.opcode `is` [CSRRW,CSRRS,CSRRC] .&&. instr.csr === 0xb02 then do
        nextRD <== fromList
              [ instret | i <- [0..valueOf @WarpSize - 1]]
      else if instr.opcode `is` [POPLEVEL] then do
        level <== inputs.peek.level - 1
      else if instr.opcode `is` [PUSHLEVEL] then do
        level <== inputs.peek.level + 1
      else do
        nextRD <== fromList [r.rd | r <- results]
      nextPC <== fromList [r.pc | r <- results]

      arbiter2.request
      when (arbiter2.grant) do
        inputs.consume
        outputs.enq
          Exec2WB
            { pc
            , warp
            , mask
            , instr
            , level= level.val
            , nextPc= nextPC.val
            , rd= nextRD.val }

  return (toSource outputs, master)

makeWriteBack :: Source Exec2WB -> WriteBack -> Module (Source WB2Select, Bit 32)
makeWriteBack inputs writeBack = do
  let instr = inputs.peek.instr
  let warp = inputs.peek.warp
  let mask = inputs.peek.mask

  cycle :: Reg (Bit 32) <- makeReg 0
  always $ cycle <== cycle.val + 1

  instret :: Reg (Bit 32) <- makeReg 0

  return
    ( Source
      { peek=
        WB2Select
          { warp= inputs.peek.warp
          , mask= inputs.peek.mask
          , level= inputs.peek.level
          , pc= inputs.peek.nextPc }
      , consume= do
          inputs.consume
          instret <== instret.val + sumList [zeroExtend a | a <- toBitList inputs.peek.mask]

          when (instr.rd.valid) do
            writeBack warp mask instr.rd.val inputs.peek.rd

          when debug do
            display
              "[" warp "@" cycle.val ":" instret.val "] pc= 0x"
              (formatHex 0 inputs.peek.pc) " "  (fshow instr)

            when (instr.rd.valid) do
              display_ "        " (fshowRegId instr.rd.val) " :="
              forM_ [0..valueOf @WarpSize-1] \ i -> do
                if mask!i
                then display_ " 0x" (formatHex 8 (inputs.peek.rd!i))
                else display_ " 0xXXXXXXXX"
              display ""
      , canPeek = inputs.canPeek }
    , instret.val )

makeGpu :: Bit 1 -> Module (Bit 1, Bit 8)
makeGpu rx = mdo
  select2fetch <- withName "select" $ makeSelect wb2select
  (exec2wb, dmaster) <- withName "exec" $ makeExec @TLConfigSimt instret dcacheSource rr2exec
  (wb2select, instret) <- withName "wite_back" $ makeWriteBack exec2wb writeBack
  (rr2exec, writeBack) <- withName "register_read" $ makeRegisterRead decode2rr
  decode2rr <- withName "decode" $ makeDecode fetch2decode
  (fetch2decode, imaster) <- withName "fetch" $ makeFetch @TLConfigCached icacheSource select2fetch

  cycle :: Reg (Bit 32) <- makeReg 0
  always do
    cycle <== cycle.val + 1

  makeDCacheSlave imaster dmaster

  pure (1, zeroExtend fetch2decode.canPeek)

dcacheSource = 0
icacheSource = 1
type TLConfigCached = TLParams 32 4 4 8 8
type TLConfigUncached = TLParams 32 4 4 8 0
type TLConfigSimt = TLParams 32 (4*WarpSize) 4 8 8

makeDCacheSlave :: TLMaster TLConfigCached -> TLMaster TLConfigSimt -> Module ()
makeDCacheSlave instrMaster simtMaster = do
  let xbarconfig =
        XBarConfig
          { bce= True
          , rootAddr= \ _ -> 0
          , rootSink= \ _ -> 0
          , rootSource= \ x ->
              select
                [ x === dcacheSource --> 0
                , x === icacheSource --> 1 ]
          , sizeChannelA= 2
          , sizeChannelB= 2
          , sizeChannelC= 2
          , sizeChannelD= 2
          , sizeChannelE= 2 }

  ([coherentMaster], [dataSlave, instrSlave]) <-
    withName "xbar" $ makeTLXBar @1 @2 @TLConfigCached xbarconfig
  makeConnection instrMaster instrSlave

  let bconfig =
        BroadcastConfig
          { sources=
              [ dcacheSource
              , icacheSource ]
          , logSize= 6
          , baseSink= 0 }
  (coherentSlave, uncoherentMaster) <-
    withName "broadcast" $ makeBroadcast @TLConfigCached bconfig

  let config =
        TLRAMConfig
          { fileName= Just "IMem.hex"
          , lowerBound= 0x80000000
          , bypassChannelA= False
          , bypassChannelD= False
          , sink= 0 }
  uncoherentSlave <- makeTLRAM @28 @TLConfigUncached config
  makeConnection uncoherentMaster uncoherentSlave
  makeConnection coherentMaster coherentSlave
  makeDecreaseWidth True simtMaster dataSlave
