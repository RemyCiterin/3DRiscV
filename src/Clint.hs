module Clint where

import Blarney
import TileLink
import TileLink.Mmio

import Utils

data ClintIfc =
  ClintIfc
    { timerInterrupt :: Bit 1
    , softwareInterrupt :: Bit 1 }

makeClint :: forall p.
  ( 32 ~ (AddrWidth p)
  , 32 ~ 8*(LaneWidth p)
  , 4 ~ (LaneWidth p)
  , KnownTLParams p)
    => Bit (SinkWidth p)
    -> Bit (AddrWidth p)
    -> Module (TLSlave p, ClintIfc)
makeClint sink base = do
  mtimecmp :: Reg (Bit 64) <- makeReg 0
  mtime :: Reg (Bit 64) <- makeReg 0
  msip :: Reg (Bit 1) <- makeReg 0

  always do
    mtime <== mtime.val + 1

  let msipMmio =
        Mmio
          { read= zeroExtend msip.val
          , write= \ lane mask -> do
              when (at @0 mask === 1) do
                msip <== at @0 lane
          , address= base }

  let mtimecmpMmio = regToMmio (base + 0x4000) (lowerReg mtimecmp)
  let mtimecmphMmio = regToMmio (base + 0x4004) (upperReg mtimecmp)

  let mtimeMmio = readOnlyMmio (base + 0xbff8) (lower mtime.val)
  let mtimehMmio = readOnlyMmio (base + 0xbffc) (upper mtime.val)

  slave <-
    makeTLMmio @p
      sink
      [ msipMmio
      , mtimeMmio
      , mtimehMmio
      , mtimecmpMmio
      , mtimecmphMmio ]

  let ifc =
        ClintIfc
          { timerInterrupt= mtime.val .>=. mtimecmp.val
          , softwareInterrupt= msip.val }

  return (slave, ifc)
