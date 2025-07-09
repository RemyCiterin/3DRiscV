module MulDiv where

import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Backend.NewSMT

-- Define a N bit multiplier
makeMultiplier :: forall n idxT.
  ( KnownNat n
  , KnownNat (Log2 n)
  , idxT ~ Bit (Log2 n) )
    => Int
    -> Module (Server (Bit n, Bit n) (Bit n))
makeMultiplier steps
  | valueOf @n < steps =
    error "The number of steps per cycle must be less that the input size"
  | steps <= 0 =
    error "We must perform at least one step per cycle"
  | otherwise = do
  idle :: Reg (Bit 1) <- makeReg true
  lhs :: Reg (Bit n) <- makeReg dontCare
  rhs :: Reg (Bit n) <- makeReg dontCare
  acc :: Reg (Bit n) <- makeReg dontCare

  -- Stop multiplication asap
  let stop :: Bit 1 = lhs.val === 0 .||. rhs.val === 0

  always do
    when (inv idle.val .&&. inv stop) do
      let accList =
            [unsafeAt i rhs.val ? (lhs.val .<<. (lit (toInteger i) :: Bit n), 0)
              | i <- [0..steps-1]]
      acc <== foldl (+) acc.val accList
      rhs <== rhs.val .>>. (lit (toInteger steps) :: Bit n)
      lhs <== lhs.val .<<. (lit (toInteger steps) :: Bit n)

  doDeq :: Wire (Bit 1) <- makeWire false
  doEnq :: Wire (Bit 1) <- makeWire false

  always do
    when (doDeq.val .&&. inv doEnq.val) do
      idle <== true

    when (doEnq.val .&&. inv doDeq.val) do
      idle <== false

  let reqs =
        Sink
          { canPut= idle.val .||. doDeq.val
          , put= \ (x,y) -> do
              doEnq <== true
              -- `rhs` is divided by two at each steps, so I try to make it null asap
              -- I assmue that in a program the probability that a lot of msb are zeros is
              -- higher than having a lot of lsb to zeros (so I optimize rhs and not lhs)
              if x .<. y then do
                rhs <== x
                lhs <== y
                display (formatHex 0 x) " " (formatHex 0 y)
              else do
                display (formatHex 0 y) " " (formatHex 0 x)
                rhs <== y
                lhs <== x }

  let resps =
        Source
          { peek= acc.val
          , canPeek= inv idle.val .&&. stop
          , consume= doDeq <== true }

  return Server{reqs, resps}

--makeDivider :: Module (Server (Bit n, Bit n) (Bit n, Bit n))

makeCheckerMultiplier :: Module ()
makeCheckerMultiplier = mdo
  Server{reqs, resps} <- makeMultiplier @8 1

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  progress :: Wire (Bit 1) <- makeWire false
  idleCount :: Reg (Bit 8) <- makeReg 0
  always (when (inv progress.val) (idleCount <== idleCount.val + 1))
  always do
    assert (idleCount.val .<. 16) "forward progress"

  queue :: Queue (Bit 8) <- makeQueue

  runStmt do
    while true do
      wait (reqs.canPut .&&. queue.notFull)
      action do
        let (x, y) = (var "x", var "y")
        queue.enq (x * y)
        reqs.put (x, y)

      wait (resps.canPeek .&&. queue.canDeq)
      action do
        progress <== true
        assert (queue.first === resps.peek) "corect output"
        display queue.first " " resps.peek
        resps.consume
        queue.deq

verifyMultiplier :: IO ()
verifyMultiplier = do
  --let conf = dfltVerifyConf { verifyConfMode = Bounded (Range 1 50) }
  --verifyWith conf makeCheckerMultiplier
  checkAuto Verbose makeCheckerMultiplier


makeTestMultiplier :: Bit 1 -> Module (Bit 1)
makeTestMultiplier _ = do
  Server{reqs,resps} <- makeMultiplier @64 1

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  runStmt do
    wait reqs.canPut
    action do
      reqs.put (12786, 763)
      display cycle.val

    wait resps.canPeek
    action do
      if toSigned resps.peek .<. toSigned 0 then do
        display "-" (-resps.peek)
      else do
        display resps.peek
      resps.consume
      display cycle.val

  return resps.canPeek
