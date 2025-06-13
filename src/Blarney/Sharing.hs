module Blarney.Sharing where

import Data.Proxy

import Blarney
import Blarney.Ehr
import Blarney.Queue
import Blarney.SourceSink

-- Transform a sink into multiple shared sink, with a synchronisation such that
-- only one actor can put into the sink per cycle. In addition each actor have
-- a put condition given as a list
makeSharedSinkWith ::
  [Bit 1] -> Sink t -> Module [Sink t]
makeSharedSinkWith [] sink = pure []
makeSharedSinkWith (x : xs) sink = do
  doPut :: Wire (Bit 1) <- makeWire false
  sinks <- makeSharedSinkWith xs sink{canPut=inv doPut.val .&&. sink.canPut}
  return (Sink{put= \m -> do doPut <== true; sink.put m, canPut= sink.canPut .&&. x} : sinks)


-- Transform a sink into multiple shared sink, with a synchronisation such that
-- only one actor can put into the sink per cycle
makeSharedSink ::
  Int -> Sink t -> Module [Sink t]
makeSharedSink size = makeSharedSinkWith (replicate size true)

makeSharedSourceWith ::
  [Bit 1] -> Source t -> Module [Source t]
makeSharedSourceWith [] source = pure []
makeSharedSourceWith (x : xs) source = do
  doPeek :: Wire (Bit 1) <- makeWire false
  sources <-
    makeSharedSourceWith xs source{canPeek= source.canPeek .&&. inv doPeek.val}

  return
    (source{consume= do doPeek <== true; source.consume, canPeek= source.canPeek .&&. x} : sources)

-- Transform a source into multiple shared sourecs, with a synchronisation such that
-- only one actor can consume from this source per cycle
makeSharedSource ::
  Int -> Source t -> Module [Source t]
makeSharedSource size = makeSharedSourceWith (replicate size true)

-- return a ram block that ensure that the output value is never corrupted
-- by a load from another actor using the same block ram
makeSafeRAM :: (Bits idx, Bits val) => RAM idx val -> Module (RAM idx val)
makeSafeRAM ram = do
  saved :: Reg val <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeDReg false

  always do
    when valid.val do
      saved <== ram.out

  return
    RAM
      { load= \ index -> do
          ram.load index
          valid <== true
      , store= ram.store
      , storeActive= ram.storeActive
      , out= valid.val ? (ram.out, saved.val)}

-- Return a list of RAM ports, the ports still need an arbiter to ensure we always
-- load or store one element at a time, but it ensure that the value in ram.out is
-- always the one loaded from the current port, and is not corrupted by other loads
makeSharedRAM ::
  (Bits idx, Bits val) =>
  Int -> RAM idx val -> Module [RAM idx val]
makeSharedRAM size ram = replicateM size (makeSafeRAM ram)

-- return a ram block that ensure that the output value is never corrupted
-- by a load from another actor using the same block ram
makeSafeRAMBE ::
  (KnownNat idx, KnownNat val, KnownNat (8*val))
    => RAMBE idx val
    -> Module (RAMBE idx val)
makeSafeRAMBE ram = do
  saved :: Reg (Bit (8*val)) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeDReg false

  always do
    when valid.val do
      saved <== ram.outBE

  return
    RAMBE
      { loadBE= \ index -> do
          ram.loadBE index
          valid <== true
      , storeBE= ram.storeBE
      , storeActiveBE= ram.storeActiveBE
      , outBE= valid.val ? (ram.outBE, saved.val)}

-- Return a list of RAM ports, the ports still need an arbiter to ensure we always
-- load or store one element at a time, but it ensure that the value in ram.out is
-- always the one loaded from the current port, and is not corrupted by other loads
makeSharedRAMBE ::
  (KnownNat idx, KnownNat val, KnownNat (8*val)) =>
  Int -> RAMBE idx val -> Module [RAMBE idx val]
makeSharedRAMBE size ram = replicateM size (makeSafeRAMBE ram)
