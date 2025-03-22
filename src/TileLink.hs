module TileLink where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Vector

import Data.Proxy

data TileLinkParamsKind =
  TileLinkParams
    Nat -- size width (in bits)
    Nat -- addr width (in bits)
    Nat -- beat width (in bytes)
    Nat -- source id
    Nat -- sink id

type family AddrWidth params where
  AddrWidth (TileLinkParams z a w o i) = a

type family BeatWidth params where
  BeatWidth (TileLinkParams z a w o i) = w

type family SizeWidth params where
  SizeWidth (TileLinkParams z a w o i) = z

type family SourceWidth params where
  SourceWidth (TileLinkParams z a w o i) = o

type family SinkWidth params where
  SinkWidth (TileLinkParams z a w o i) = i

type KnownNatTileLinkParams (p :: TileLinkParamsKind) =
  ( KnownNat (AddrWidth p)
  , KnownNat (BeatWidth p)
  , KnownNat (SizeWidth p)
  , KnownNat (SourceWidth p)
  , KnownNat (SinkWidth p))



newtype AParam = AParam (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype BParam = BParam (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype CParam = CParam (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype DParam = DParam (Bit 2)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)



newtype AOpcode = AOpcode (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype BOpcode = BOpcode (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype COpcode = COpcode (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)

newtype DOpcode = DOpcode (Bit 3)
  deriving stock Generic
  deriving newtype Interface
  deriving anyclass (Cmp, Bits, FShow)



