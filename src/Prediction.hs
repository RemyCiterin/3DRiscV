module Prediction where

import Instr
import Blarney
import Blarney.Ehr
import Blarney.Queue
import Blarney.Option
import Blarney.Utils
import Data.Proxy
import Blarney.ADT

data JumpEnum
  = Call
  | Ret
  | Branch
  | Jump
  | Linear
  deriving(Bounded, Enum)

type JumpKind = Bit 3

getJumpKind :: Instr -> JumpKind
getJumpKind instr =
  selectDefault (enum Linear) [
    instr.opcode `is` [BEQ,BNE,BLT,BLTU,BGE,BGEU] --> enum Branch,
    instr.opcode `is` [JALR] --> jalrKind,
    instr.opcode `is` [JAL] --> enum Jump
  ]
    where
      isRet = instr.rs1.val === 1 .||. instr.rs1.val === 5
      isCall = instr.rd.val === 1 .||. instr.rd.val === 5

      jalrKind =
        selectDefault (enum Jump) [
          isCall --> enum Call,
          inv isCall .&&. isRet --> enum Ret
        ]

getJumpKindOption :: Option Instr -> JumpKind
getJumpKindOption instr = instr.valid ? (getJumpKind instr.val, enum Linear)

-- Predict the output of a return instruction
-- using the history of all the previous call/ret
-- instructions, this version only backtrack the
-- position on the stack, not the values
data ReturnAddressStack size = ReturnAddressStack
  { top :: Bit size
  , predict :: Bit 32 -> JumpKind -> Action (Option (Bit 32))
  , backtrack :: Bit size -> JumpKind -> Action ()}

makeReturnAddressStack :: KnownNat size => Module (ReturnAddressStack size)
makeReturnAddressStack = do
  -- initialised with none
  stack :: RAM (Bit size) (Option (Bit 32)) <- makeDualRAM

  head :: Reg (Bit size) <- makeReg 0
  headPred :: Wire (Bit size) <- makeWire dontCare
  headBack :: Wire (Bit size) <- makeWire dontCare

  always do
    let newHead = headBack.active ? (headBack.val, headPred.active ? (headPred.val, head.val))
    stack.load (newHead - 1)
    head <== newHead

  return ReturnAddressStack
    { top= head.val
    , predict= \ pc kind -> do
        when (kind === enum Call) do
          stack.store head.val (some $ pc+4)
          headPred <== head.val + 1

        when (kind === enum Ret) do
          headPred <== head.val - 1

        return $ kind === enum Ret ? (stack.out, none)

    , backtrack= \ index kind -> do
        if (kind === enum Call) then do
          headBack <== index + 1
        else if (kind === enum Ret) then do
          headBack <== index - 1
        else do
          headBack <== index }

-- Predict the address of a call/jump/branch instruction
-- and remember the kind of all jump/call/ret/branch
-- instructions
data BranchTargetBuffer =
  BranchTargetBuffer
    { start :: Bit 32 -> Action ()
    , predict :: (Bit 32, JumpKind)
    , train :: Bit 32 -> Bit 32 -> JumpKind -> Action ()}

makeBranchTargetBuffer :: Int -> Module BranchTargetBuffer
makeBranchTargetBuffer logSize = do
  liftNat logSize $ \ (_ :: Proxy aw) -> do

    ram :: RAM (Bit aw) (Option (Bit 32, Bit 32, JumpKind)) <- makeDualRAM
    currentPc :: Reg (Bit 32) <- makeReg dontCare

    let hash :: Bit 32 -> Bit aw = \ addr -> truncateCast (slice @31 @2 addr)

    return BranchTargetBuffer
      { start= \ pc -> do
          ram.load (hash pc)
          currentPc <== pc

      , predict=
          let pc = currentPc.val in
          let (expected, next, kind) = ram.out.val in
          (ram.out.valid .&&. expected === pc) ? ( (next, kind) , (pc+4, enum Linear) )

      , train= \ pc next kind -> when (next =!= pc + 4) do
          ram.store (hash pc) (some (pc, next, kind))}

data GhtState =
  GhtState
    { base :: Bit 3
    , pred :: Bit 3
    , found :: Bit 1}
    deriving(Bits, Generic)

-- Predict the direction of a branch instruction,
-- for each branch instruction such we must train
-- the predictor with the program counter and
-- history at the time the branch was seen
data GlobalHistoryTable size =
  GlobalHistoryTable
    { start :: Bit 32 -> Bit size -> Action ()
    , predict :: (Bit 1, GhtState)
    , train :: Bit 32 -> Bit size -> Bit 1 -> GhtState -> Action ()}

-- saturation signed increment
satSignedIncr :: KnownNat n => Bit n -> Bit n
satSignedIncr x = (toSigned (x+1) .>. toSigned x) ? (x+1, x)

-- saturation signed decrement
satSignedDecr :: KnownNat n => Bit n -> Bit n
satSignedDecr x = (toSigned (x-1) .<. toSigned x) ? (x-1, x)

makeGlobalHistoryTable :: forall size. KnownNat size => Module (GlobalHistoryTable size)
makeGlobalHistoryTable = do
  let hash :: Bit 32 -> Bit size = \ pc -> truncateCast (slice @31 @2 pc)
  let hashWith :: Bit 32 -> Bit size -> Bit size = \ pc hist -> hash pc .^. hist

  baseRam :: RAM (Bit size) (Bit 3) <- makeDualRAM -- init with zeros
  predRam :: RAM (Bit size) (Option (Bit size, Bit 3)) <- makeDualRAM -- init with `none`

  currentPc :: Reg (Bit 32) <- makeReg dontCare

  return GlobalHistoryTable
    { start= \ pc hist -> do
        baseRam.load (hash pc)
        predRam.load (hashWith pc hist)
        currentPc <== pc

    , predict=
        let pc = currentPc.val in
        let (tag, pred) = predRam.out.val in
        let found = predRam.out.valid .&&. hash pc === tag in
        let out :: Bit 3 = found ? (pred, baseRam.out) in
        (toSigned out .>=. toSigned 0, GhtState{base= baseRam.out, pred=found ? (pred,0), found})

    , train= \ pc hist taken state -> do
        let base = taken ? (satSignedIncr state.base, satSignedDecr state.base)
        let pred = taken ? (satSignedIncr state.pred, satSignedDecr state.pred)

        predRam.store (hashWith pc hist) (some (hash pc, pred))
        baseRam.store (hash pc) base}

data BPredState histSize rasSize =
  BPredState
    { hist :: Bit histSize
    , kind :: JumpKind
    , top :: Bit rasSize
    , state :: GhtState}
    deriving(Generic, Bits)

data BranchPredictor histSize rasSize epochSize =
  BranchPredictor
    { start :: Bit 32 -> Bit epochSize -> Action ()
    , request :: (Bit 32, Bit epochSize)
    , predict :: Action (Bit 32, BPredState histSize rasSize)
    , trainHit :: BPredState histSize rasSize -> Bit 32 -> Bit 32 -> Option Instr -> Action ()
    , trainMis :: BPredState histSize rasSize -> Bit 32 -> Bit 32 -> Option Instr -> Action ()}

makeBranchPredictor ::
  forall histSize rasSize epochSize.
  (KnownNat histSize, KnownNat rasSize, KnownNat epochSize) =>
  Int -> Module (BranchPredictor histSize rasSize epochSize)
makeBranchPredictor logBtbSize = do
  btb <- makeBranchTargetBuffer logBtbSize
  ght <- makeGlobalHistoryTable
  ras <- makeReturnAddressStack
  hist :: Ehr (Bit histSize) <- makeEhr 3 0
  currentPc :: Reg (Bit 32) <- makeReg dontCare
  currentEpoch :: Reg (Bit epochSize) <- makeReg dontCare

  let update :: Bit 1 -> Bit histSize -> Bit histSize = \ taken hist ->
        (hist .<<. (1 :: Bit 1)) .|. zeroExtendCast taken

  return BranchPredictor
    { start= \ pc epoch -> do
        currentEpoch <== epoch
        currentPc <== pc

        ght.start pc (hist.read 1)
        btb.start pc

    , request= (currentPc.val, currentEpoch.val)

    , predict= do
        let pc = currentPc.val
        let (btbPc, kind) = btb.predict
        let (branchTaken, state) = ght.predict
        let taken = branchTaken .||. kind =!= enum Branch

        retPc <- ras.predict pc kind
        let branchPc = retPc.valid ? (retPc.val, btbPc)

        when (branchPc =!= pc + 4 .&&. kind =!= enum Branch) do
          hist.write 0 (update taken (hist.read 0))

        return (taken ? (branchPc, pc + 4), BPredState{
          hist= hist.read 0,
          top= ras.top,
          state,
          kind
        })

    , trainHit= \ state pc next instr -> do
        let kind = getJumpKindOption instr
        let taken = next =!= pc + 4

        when (kind === enum Branch) do
          ght.train pc state.hist taken state.state

    , trainMis= \ state pc next instr -> do
        let kind = getJumpKindOption instr
        let taken = next =!= pc + 4

        when (kind === enum Branch) do
          ght.train pc state.hist taken state.state

        let newHist = update taken state.hist

        if instr.valid then do
          hist.write 2 (kind === enum Branch ? (newHist, state.hist))
          btb.train pc next kind
          ras.backtrack state.top kind
        else do
          ras.backtrack state.top state.kind
          hist.write 2 state.hist
    }
