module TileLink where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Vector

import Data.Proxy

data TLParamsKind =
  TLParams
    Nat -- size width (in bits)
    Nat -- addr width (in bits)
    Nat -- lane width (in bytes)
    Nat -- source id
    Nat -- sink id

type family AddrWidth params where
  AddrWidth (TLParams z a w o i) = a

type family LaneWidth params where
  LaneWidth (TLParams z a w o i) = w

type family SizeWidth params where
  SizeWidth (TLParams z a w o i) = z

type family SourceWidth params where
  SourceWidth (TLParams z a w o i) = o

type family SinkWidth params where
  SinkWidth (TLParams z a w o i) = i

type KnownNatTLParams (p :: TLParamsKind) =
  ( KnownNat (AddrWidth p)
  , KnownNat (LaneWidth p)
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

