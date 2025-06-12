module Blarney.Sharing where

import Data.Proxy

import Blarney
import Blarney.Ehr
import Blarney.Queue
import Blarney.SourceSink

-- Transform a sink into multiple shared sink, with a synchronisation such that
-- only one actor can put into the sink per cycle
makeSharedSink ::
  Int -> Sink t -> Module [Sink t]
makeSharedSink size sink = do
  doPut :: [Wire (Bit 1)] <- replicateM size (makeWire false)

  let canPut i =
        if i == 0
           then sink.canPut
           else inv (doPut!(i-1)).val .&&. canPut (i-1)

  return
    [
      Sink
        { canPut= canPut i
        , put= \x -> do
            (doPut!i) <== true
            sink.put x }
    | i <- [0..size-1]]

-- Transform a source into multiple shared sourecs, with a synchronisation such that
-- only one actor can consume from this source per cycle
makeSharedSource ::
  Int -> Source t -> Module [Source t]
makeSharedSource size source = do
  doPeek :: [Wire (Bit 1)] <- replicateM size (makeWire false)

  let canPeek i =
        if i == 0
           then source.canPeek
           else inv (doPeek!(i-1)).val .&&. canPeek (i-1)

  return
    [
      Source
        { canPeek= canPeek i
        , peek= source.peek
        , consume= do
            source.consume
            (doPeek!i) <== true}
    | i <- [0..size-1]]

-- Return a list of RAM ports, the ports still need an arbiter to ensure we always
-- load or store one element at a time, but it ensure that the value in ram.out is
-- always the one loaded from the current port, and is not corrupted by other loads
makeSharedRAM ::
  (Bits idx, Bits val) =>
  Int -> RAM idx val -> Module [RAM idx val]
makeSharedRAM size ram = do
  saved :: [Reg val] <- replicateM size (makeReg dontCare)
  valid :: [Reg (Bit 1)] <- replicateM size (makeDReg false)

  always do
    sequence_ [when cond.val (reg <== ram.out) | (cond,reg) <- zip valid saved]

  return
    [
      RAM
        { load= \ index -> do
            valid!i <== true
            ram.load index
        , store= ram.store
        , storeActive= ram.storeActive
        , out= (valid!i).val ? (ram.out, (saved!i).val)}
    | i <- [0..toInteger size-1]]

-- Return a list of RAM ports, the ports still need an arbiter to ensure we always
-- load or store one element at a time, but it ensure that the value in ram.out is
-- always the one loaded from the current port, and is not corrupted by other loads
makeSharedRAMBE ::
  (KnownNat idx, KnownNat val, KnownNat (8*val)) =>
  Int -> RAMBE idx val -> Module [RAMBE idx val]
makeSharedRAMBE size ram = do
  saved :: [Reg (Bit (8*val))] <- replicateM size (makeReg dontCare)
  valid :: [Reg (Bit 1)] <- replicateM size (makeDReg false)

  always do
    sequence_ [when cond.val (reg <== ram.outBE) | (cond,reg) <- zip valid saved]

  return
    [
      RAMBE
        { loadBE= \ index -> do
            valid!i <== true
            ram.loadBE index
        , storeBE= ram.storeBE
        , storeActiveBE= ram.storeActiveBE
        , outBE= (valid!i).val ? (ram.outBE, (saved!i).val)}
    | i <- [0..toInteger size-1]]
