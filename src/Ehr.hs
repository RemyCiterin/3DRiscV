module Ehr where

import Blarney
import Data.Proxy

data Ehr a =
  Ehr
    { read :: Int -> a
    , write :: Int -> a -> Action ()}

makeEhr :: (Bits a) => Int -> a -> Module (Ehr a)
makeEhr number init = do
  reg :: Reg a <- makeReg init
  wires :: [Wire a] <- replicateM number (makeWire dontCare)

  always do
    reg <== readAt reg wires number

  return Ehr {
    read = \i -> readAt reg wires i,
    write = \i a -> do
      wires !! i <== a
  }

  where
    readAt reg wires n
      | n == 0 = reg.val
      | otherwise =
        (wires !! (n-1)).active ?
          ((wires !! (n-1)).val, readAt reg wires (n-1))

data Fifo a =
  Fifo
    { enq :: a -> Action ()
    , deq :: Action ()
    , first :: a
    , canEnq :: Bit 1
    , canDeq :: Bit 1}

-- implement a pipelined fifo of size one
makeOnePipelineFifo :: Bits a => Module (Fifo a)
makeOnePipelineFifo = do
  reg :: Reg a <- makeReg dontCare
  valid :: Ehr (Bit 1) <- makeEhr 2 false

  return Fifo {
    first = reg.val,
    canDeq = valid.read 0,
    deq = do
      dynamicAssert (valid.read 0) "cannot dequeue"
      valid.write 0 false,
    canEnq = inv (valid.read 1),
    enq = \x -> do
      dynamicAssert (inv $ valid.read 1) "cannot enqueue"
      valid.write 1 true
      reg <== x
  }

makeSizedPipelineFifo :: Bits a => Int -> Module (Fifo a)
makeSizedPipelineFifo size =
  liftNat (log2ceil size) \(_ :: Proxy logSizeT) -> do
    let max_index :: Bit logSizeT = fromInteger (toInteger size - 1)

    fifo :: [Reg a] <- replicateM size (makeReg dontCare)
    head :: Ehr (Bit logSizeT) <- makeEhr 2 0
    tail :: Ehr (Bit logSizeT) <- makeEhr 2 0
    empty :: Ehr (Bit 1) <- makeEhr 2 1
    full :: Ehr (Bit 1) <- makeEhr 2 0

    return Fifo {
      first = listIndex (head.read 0) [x.val | x <- fifo],
      canDeq = inv (empty.read 0),
      deq = do
        dynamicAssert (inv (empty.read 0)) "cannot dequeue"
        let hd :: Bit logSizeT = head.read 0
        let tl :: Bit logSizeT = tail.read 0

        full.write 0 0
        let new_head :: Bit logSizeT = (hd .==. max_index) ? (0, hd+1)
        when (new_head .==. tl) do
          empty.write 0 1

        head.write 0 new_head,
      canEnq = inv (full.read 1),
      enq = \ x -> do
        dynamicAssert (inv (full.read 1)) "cannot enqueue"
        let hd :: Bit logSizeT = head.read 1
        let tl :: Bit logSizeT = tail.read 1

        forM [0..size-1] \i -> when (tl .==. fromInteger (toInteger i)) do
          (fifo ! i) <== x

        empty.write 1 0
        let new_tail :: Bit logSizeT = (tl .==. max_index) ? (0, tl+1)
        when (new_tail .==. hd) do
          full.write 1 1

        tail.write 1 new_tail
    }

makePipelineFifo :: Bits a => Int -> Module (Fifo a)
makePipelineFifo size
  | size == 1 = makeOnePipelineFifo
  | otherwise = makeSizedPipelineFifo size


-- implement a bypass fifo of size one
makeOneBypassFifo :: Bits a => Module (Fifo a)
makeOneBypassFifo = do
  ehr :: Ehr a <- makeEhr 2 dontCare
  valid :: Ehr (Bit 1) <- makeEhr 2 false

  return Fifo {
    first = ehr.read 1,
    canDeq = valid.read 1,
    deq = do
      dynamicAssert (valid.read 1) "cannot dequeue"
      valid.write 1 false,
    canEnq = inv (valid.read 0),
    enq = \x -> do
      dynamicAssert (inv $ valid.read 0) "cannot enqueue"
      valid.write 0 true
      ehr.write 0 x
  }

makeSizedBypassFifo :: Bits a => Int -> Module (Fifo a)
makeSizedBypassFifo size =
  liftNat (log2ceil size) \(_ :: Proxy logSizeT) -> do
    let max_index :: Bit logSizeT = fromInteger (toInteger size - 1)

    fifo :: [Reg a] <- replicateM size (makeReg dontCare)
    head :: Ehr (Bit logSizeT) <- makeEhr 2 0
    tail :: Ehr (Bit logSizeT) <- makeEhr 2 0
    empty :: Ehr (Bit 1) <- makeEhr 2 1
    full :: Ehr (Bit 1) <- makeEhr 2 0
    wire :: Wire a <- makeWire dontCare

    return Fifo {
      canEnq = inv (full.read 0),
      enq = \ x -> do
        dynamicAssert (inv (full.read 0)) "cannot enqueue"
        let hd :: Bit logSizeT = head.read 0
        let tl :: Bit logSizeT = tail.read 0

        wire <== x
        forM [0..size-1] \i -> when (tl .==. fromInteger (toInteger i)) do
          (fifo ! i) <== x

        empty.write 0 0
        let new_tail :: Bit logSizeT = (tl .==. max_index) ? (0, tl+1)
        when (new_tail .==. hd) do
          full.write 0 1

        tail.write 0 new_tail,
      first =
        (wire.active .&&. tail.read 0 .==. head.read 1) ?
          (wire.val, listIndex (head.read 1) [x.val | x <- fifo]),
      canDeq = inv (empty.read 1),
      deq = do
        dynamicAssert (inv (empty.read 1)) "cannot dequeue"
        let hd :: Bit logSizeT = head.read 1
        let tl :: Bit logSizeT = tail.read 1

        full.write 1 0
        let new_head :: Bit logSizeT = (hd .==. max_index) ? (0, hd+1)
        when (new_head .==. tl) do
          empty.write 1 1

        head.write 1 new_head
    }


makeBypassFifo :: Bits a => Int -> Module (Fifo a)
makeBypassFifo size
  | size == 1 = makeOneBypassFifo
  | otherwise = makeSizedBypassFifo size
