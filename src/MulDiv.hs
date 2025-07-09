module MulDiv where

import Blarney
import Blarney.Stmt
import Blarney.Queue
import Blarney.Option
import Blarney.SourceSink
import Blarney.ClientServer
import Blarney.Backend.NewSMT

-- Define a N bit multiplier
makeMultiplier :: forall n.
  KnownNat n
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
                lhs <== x
              acc <== 0 }

  let resps =
        Source
          { peek= acc.val
          , canPeek= inv idle.val .&&. stop
          , consume= doDeq <== true }

  return Server{reqs, resps}

makeDivider :: forall n.
  (KnownNat n)
    => Module (Server (Bit n, Bit n) (Bit n, Bit n))
makeDivider = do
  idle <- makeReg true
  div <- makeReg (dontCare :: Bit n)
  rem <- makeReg (dontCare :: Bit n)
  index <- makeReg (dontCare :: Bit n)

  num <- makeReg (dontCare :: Bit n)
  den <- makeReg (dontCare :: Bit n)

  always do
    -- Let a = b * (2^-(n+1) * q) + r
    --    2 * a = b * (2 ^ -n * q) + (2 * r)
    --    2 * a + 1 = b * (2^-n * q) + (2 * r + 1)
    when (inv idle.val .&&. index.val =!= 0) do
      let remTimes2 = rem.val .<<. (1 :: Bit n)
      let newRem =
            (num.val .&. index.val =!= 0) ?
              (remTimes2 .|. 1, remTimes2)

      if (newRem .>=. den.val) then do
        div <== div.val .|. index.val
        rem <== newRem - den.val
      else do
        rem <== newRem

      index <== index.val .>>. (1 :: Bit n)

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
          , put= \ (x, y) -> do
              index <== lit (2 ^ (valueOf @n - 1))
              doEnq <== true
              num <== x
              den <== y
              rem <== 0
              div <== 0 }

  let resps =
        Source
          { peek= (div.val, rem.val)
          , canPeek= inv idle.val .&&. index.val === 0
          , consume= doDeq <== true }

  return Server{reqs,resps}

-- Detect a signed division overflow (-minInt is not representable)
signedDivOverflow :: forall n. KnownNat n => (Bit n, Bit n) -> Bit 1
signedDivOverflow (num, den) =
  den === -1 .&&. num === lit (- 2 ^ (valueOf @n - 1))

forwardProgress = True

makeCheckerMultiplier :: Module ()
makeCheckerMultiplier = mdo
  Server{reqs, resps} <- makeMultiplier @8 2

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  progress :: Wire (Bit 1) <- makeWire false
  idleCount :: Reg (Bit 8) <- makeReg 0
  always do
    idleCount <== progress.val ? (0, idleCount.val + 1)
    when forwardProgress do
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

makeTestDivider :: Bit 1 -> Module (Bit 1)
makeTestDivider _ = do
  Server{reqs,resps} <- makeDivider @32

  cycle :: Reg (Bit 32) <- makeReg 0
  always (cycle <== cycle.val + 1)

  runStmt do
    wait reqs.canPut
    action do
      reqs.put (12786, 7)
      display cycle.val

    wait resps.canPeek
    action do
      if toSigned resps.peek.fst .<. toSigned 0 then do
        display "-" (-resps.peek.fst)
      else do
        display resps.peek.fst
      if toSigned resps.peek.snd .<. toSigned 0 then do
        display "-" (-resps.peek.snd)
      else do
        display resps.peek.snd
      resps.consume
      display cycle.val

  return resps.canPeek
