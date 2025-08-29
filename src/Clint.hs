module Clint where

import Blarney
import TileLink
import TileLink.Mmio

import Blarney.Utils

data ClintIfc =
  ClintIfc
    { timerInterrupt :: [Bit 1]
    , softwareInterrupt :: Bit 1 }

makeClint :: forall p.
  ( 32 ~ (AddrWidth p)
  , 32 ~ 8*(LaneWidth p)
  , 4 ~ (LaneWidth p)
  , KnownTLParams p)
    => Int
    -> Bit (AddrWidth p)
    -> Module ([Mmio p], ClintIfc)
makeClint numHart base = do
  mtimecmp :: [Reg (Bit 64)] <- replicateM numHart (makeReg 0)
  mtime :: Reg (Bit 64) <- makeReg 0
  msip :: Reg (Bit 1) <- makeReg 0

  always do
    mtime <== mtime.val + 1

  let msipMmio =
        Mmio
          { read= const (pure (zeroExtend msip.val))
          , write= \ lane mask -> do
              when (at @0 mask === 1) do
                msip <== at @0 lane
          , address= base }

  let mtimecmpMmio =
        [regToMmio (base + 0x4000 + 8 * lit hartId) (lowerReg cmp)
          | (hartId, cmp) <- zip [0..] mtimecmp]
  let mtimecmphMmio =
        [regToMmio (base + 0x4004 + 8 * lit hartId) (upperReg cmp)
          | (hartId, cmp) <- zip [0..] mtimecmp]

  let mtimeMmio = readOnlyMmio (base + 0xbff8) (lower mtime.val)
  let mtimehMmio = readOnlyMmio (base + 0xbffc) (upper mtime.val)

  always do
    when ((mtimecmp!!0).val =!= (buffer (mtimecmp!!0).val)) do
      display "update clint"

  let mmioList =
        [ msipMmio
        , mtimeMmio
        , mtimehMmio ]
        ++ mtimecmpMmio
        ++ mtimecmphMmio

  let ifc =
        ClintIfc
          { timerInterrupt= [mtime.val .>=. cmp.val | cmp <- mtimecmp]
          , softwareInterrupt= msip.val }

  return (mmioList, ifc)
