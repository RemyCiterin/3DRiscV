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
import Blarney.QuadPortRAM
import Blarney.Stmt
import Blarney.ADT

import System
import Instr
import Clint
import Uart
import Alu
import CSR

import TileLink
import TileLink.CoherentBCache
import TileLink.Broadcast

debug :: Bool
debug = True

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

type WarpSize = 2
type WarpMask = Bit WarpSize
type WarpIdx = Bit (Log2Ceil WarpSize)

data Select2Fetch =
  Select2Fetch
    { warp :: WarpId
    , mask :: WarpMask
    , pc :: Bit 32 }
  deriving(Generic, Bits)

data Fetch2Decode =
  Fetch2Decode
    { warp :: WarpId
    , mask :: WarpMask
    , pc :: Bit 32
    , instr :: Bit 32 }
  deriving(Generic, Bits)

data Decode2RR =
  Decode2RR
    { warp :: WarpId
    , mask :: WarpMask
    , pc :: Bit 32
    , instr :: Instr }
  deriving(Generic, Bits)

data RR2Exec =
  RR2Exec
    { warp :: WarpId
    , mask :: WarpMask
    , pc :: Bit 32
    , instr :: Instr
    , op1 :: Vec WarpSize (Bit 32)
    , op2 :: Vec WarpSize (Bit 32) }
  deriving(Generic, Bits)

data Exec2WB =
  Exec2WB
    { warp :: WarpId
    , mask :: WarpMask
    , instr :: Instr
    , rd :: Vec WarpSize (Bit 32)
    , nextPc :: Vec WarpSize (Bit 32)
    , pc :: Bit 32 }
  deriving(Generic, Bits)

data WB2Select =
  WB2Select
    { warp :: WarpId
    , mask :: WarpMask
    , pc :: Vec WarpSize (Bit 32) }
  deriving(Generic, Bits)

type WriteBack = WarpId -> WarpMask -> RegId -> Vec WarpSize (Bit 32) -> Action ()

makeFetch :: Source Select2Fetch -> Module (Source Fetch2Decode)
makeFetch inputs = do
  imem :: RAM (Bit 16) (Bit 32) <- makeRAMInit "Mem.hex"

  queue :: Queue Select2Fetch <- makePipelineQueue 1

  always do
    when (inputs.canPeek .&&. queue.notFull) do
      imem.load (slice @17 @2 inputs.peek.pc)
      queue.enq inputs.peek
      inputs.consume

  return
    Source
      { canPeek= queue.canDeq
      , consume= queue.deq
      , peek=
          Fetch2Decode
            { warp= queue.first.warp
            , mask= queue.first.mask
            , pc= queue.first.pc
            , instr= imem.out }}

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
          , pc= inputs.peek.pc
          , instr= decodeInstr inputs.peek.instr }

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
    , pc :: Bit 32 }
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

  always do
    -- Write back results from exec stage
    when inputs.canPeek do
      forM_ [0..valueOf @WarpSize - 1] \ i -> do
        when (inputs.peek.mask!i) do
          (statesB!i).store inputs.peek.warp SimtState{busy= false, pc= inputs.peek.pc!i}
      inputs.consume

    sequence_ [s.load (warp.read 1) | s <- statesA]

    -- Scheduler: choose the first thread availables from `round.val`
    let actives :: WarpMask = fromBitList [inv s.out.busy | s <- statesA]
    let choosen = rotr (firstHot (rotl actives round.val)) round.val

    forM_ [0..valueOf @WarpSize-1] \ i -> do
      when (choosen!i) do pc <== (statesA!i).out.pc

    when (inv initDone.val) do
      sequence_
        [ s.store initIndex.val SimtState{busy= false, pc= 0x80000000}
        | s <-statesA ]
      initDone <== initIndex.val + 1 === 0
      initIndex <== initIndex.val + 1

  let source :: Source Select2Fetch =
        Source
          { consume= do
              sequence_
                [ when (inv s.out.busy .&&. s.out.pc === pc.val) do
                  s.store (warp.read 0) SimtState{busy= true, pc= s.out.pc}
                | s <- statesA]
              warp.write 0 (1 + warp.read 0)
              when (warp.read 0 === 0) do
                round <== round.val + 1
          , peek=
              Select2Fetch
                { warp= warp.read 0
                , mask= fromBitList [inv s.out.busy .&&. s.out.pc === pc.val | s <- statesA]
                , pc= pc.val }
          , canPeek=
              orList [inv s.out.busy | s <- statesA] .&&.
              inv (inputs.canPeek .&&. warp.read 0 === inputs.peek.warp) .&&.
              initDone.val
          }

  makeConnection source (toSink queue)
  return (toSource queue)

data DMemReq =
  DMemReq
    { isStore :: Bit 1
    , width :: Bit 2
    , isUnsigned :: Bit 1
    , mask :: WarpMask
    , addr :: Vec WarpSize (Bit 32)
    , lane :: Vec WarpSize (Bit 32) }
  deriving(Generic, Bits)


makeDMemServer :: Source DMemReq -> Module (Source (Vec WarpSize (Bit 32)))
makeDMemServer inputs = do
  dmem :: RAMBE 16 4 <- makeDualRAMForwardInitBE "Mem.hex"
  outputs :: Queue (Vec WarpSize (Bit 32)) <- makePipelineQueue 1

  buffer :: [Reg (Bit 32)] <- replicateM (valueOf @WarpSize) (makeReg dontCare)

  let isByte :: Bit 2 -> Bit 1 = \ width -> width === 0b00
  let isHalf :: Bit 2 -> Bit 1 = \ width -> width === 0b01
  let isWord :: Bit 2 -> Bit 1 = \ width -> width === 0b10

  let genMask :: Bit 2 -> Bit 32 -> Bit 4 = \ width addr ->
        select [
          isByte width --> 0b0001,
          isHalf width --> 0b0011,
          isWord width --> 0b1111
        ] .<<. (slice @1 @0 addr)

  let genAligned :: Bit 2 -> Bit 32 -> Bit 1 = \ width addr ->
        select
          [ isByte width --> true
          , isHalf width --> at @0 addr === 0
          , isWord width --> slice @1 @0 addr === 0 ]

  let genLane :: Bit 32 -> Bit 32 -> Bit 32 = \ addr lane ->
        lane .<<. (slice @1 @0 addr # (0 :: Bit 3))

  let genRd :: Bit 1 -> Bit 2 -> Bit 32 -> Bit 32 -> Bit 32 = \ unsigned width addr lane ->
        select
          [ isHalf width .&&. inv unsigned --> signExtend (slice @15 @0 lane)
          , isByte width .&&. inv unsigned --> signExtend (slice @7 @0 lane)
          , isHalf width .&&. unsigned --> zeroExtend (slice @15 @0 lane)
          , isByte width .&&. unsigned --> zeroExtend (slice @7 @0 lane)
          , isWord width --> lane ]
        .>>. (slice @1 @0 addr # (0 :: Bit 3))

  runStmt do
    while true do
      wait inputs.canPeek
      let req = inputs.peek
      par
        [ forM_ [0..valueOf @WarpSize - 1] \ i -> do
            let (msb, lsb) = split (slice @31 @2 (req.addr!i - 0x80000000))
            let mask = genMask req.width (req.addr!i)

            action do
              -- Show ascii output of the gpu
              when (req.mask!i .&&. req.addr!i === 0x10000000 .&&. req.isStore .&&. at @0 mask) do
                when debug (display_ "print char: ")
                displayAscii (slice @7 @0 (req.lane!i))
                when debug (display "")

              if req.mask!i .&&. req.isStore .&&. msb === 0 .&&. genAligned req.width (req.addr!i)
              then do
                when debug do
                  display
                    "        [0x"
                    (formatHex 0 (req.addr!i .&. 0xfffffffc))
                    "] <= "
                    (formatHex 0 (genLane (req.addr!i) (req.lane!i)))
                    " if 0x"
                    (formatHex 1 mask)

                dmem.storeBE lsb mask (genLane (req.addr!i) (req.lane!i))
              else do
                when debug do
                  display "        read at addr: " (formatHex 0 (req.addr!i .&. 0xfffffffc))
                dmem.loadBE lsb
        , do
          tick
          forM_ [0..valueOf @WarpSize - 1] \ i -> do
            action do
              buffer!i <== genRd req.isUnsigned req.width (req.addr!i) dmem.outBE ]

      wait outputs.notFull
      action do
        inputs.consume
        outputs.enq (fromList [b.val | b <- buffer])

  return (toSource outputs)

makeExec :: Source RR2Exec -> Module (Source Exec2WB)
makeExec inputs = do
  -- Data memory
  dmemIn <- makeQueue
  dmemOut <- makeDMemServer (toSource dmemIn)

  -- Set of requests waiting for a memory request
  pendingQ :: Queue (WarpId, WarpMask, Instr, Bit 32) <- makeSizedQueueCore 2

  -- arbiters for the write-back stage queue between the memory/alu responses
  [arbiter1, arbiter2] <- makeFairArbiter 2

  -- return the program counters to toe scheduler
  outputs :: Queue Exec2WB <- makeQueue

  nextPC :: Wire (Vec WarpSize (Bit 32)) <- makeWire dontCare
  nextRD :: Wire (Vec WarpSize (Bit 32)) <- makeWire dontCare

  always do
    when (outputs.notFull .&&. dmemOut.canPeek .&&. pendingQ.canDeq) do
      let (warp, mask, instr, pc) = pendingQ.first

      let nextPc = fromList (replicate (valueOf @WarpSize) (pc + 4))
      let rd = dmemOut.peek

      arbiter1.request
      when (arbiter1.grant) do
        pendingQ.deq
        dmemOut.consume
        outputs.enq
          Exec2WB
            { pc
            , warp
            , mask
            , instr
            , nextPc
            , rd }

    when (inputs.canPeek .&&. outputs.notFull .&&. dmemIn.notFull .&&. pendingQ.notFull) do
      let op1 = toList inputs.peek.op1
      let op2 = toList inputs.peek.op2
      let instr = inputs.peek.instr
      let mask = inputs.peek.mask
      let warp = inputs.peek.warp
      let pc = inputs.peek.pc

      if instr.isMemAccess then do
        pendingQ.enq (warp, mask, instr, pc)
        inputs.consume
        let addr =
              fromList (map (+instr.imm.val) op1)
        dmemIn.enq
          DMemReq
            { isStore= instr.opcode `is` [STORE]
            , isUnsigned= instr.isUnsigned
            , width= instr.accessWidth
            , lane= fromList op2
            , mask= mask
            , addr }

      else do
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
          nextPC <== fromList [r.pc | r <- results]
          nextRD <== fromList
                [ 0 # warp # (lit (toInteger i) :: WarpIdx) | i <- [0..valueOf @WarpSize - 1]]
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
              , nextPc= nextPC.val
              , rd= nextRD.val }

  return (toSource outputs)

makeWriteBack :: Source Exec2WB -> WriteBack -> Module (Source WB2Select)
makeWriteBack inputs writeBack = do
  let instr = inputs.peek.instr
  let warp = inputs.peek.warp
  let mask = inputs.peek.mask

  cycle :: Reg (Bit 32) <- makeReg 0
  always $ cycle <== cycle.val + 1

  instret :: Reg (Bit 32) <- makeReg 0

  return
    Source
      { peek=
        WB2Select
          { warp= inputs.peek.warp
          , mask= inputs.peek.mask
          , pc= inputs.peek.nextPc }
      , consume= do
          inputs.consume
          instret <== instret.val + 1

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

makeGpu :: Bit 1 -> Module (Bit 1, Bit 8)
makeGpu rx = mdo
  select2fetch <- makeSelect wb2select
  exec2wb <- makeExec rr2exec
  wb2select <- makeWriteBack exec2wb writeBack
  (rr2exec, writeBack) <- makeRegisterRead decode2rr
  decode2rr <- makeDecode fetch2decode
  fetch2decode <- makeFetch select2fetch

  cycle :: Reg (Bit 32) <- makeReg 0
  always do
    cycle <== cycle.val + 1

  pure (1, 0)
