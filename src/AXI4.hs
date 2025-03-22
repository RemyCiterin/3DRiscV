module AXI4 where

import Blarney
import Blarney.SourceSink
import Blarney.Connectable
import Blarney.Vector

import Data.Proxy

data Axi4ParamsKind =
  Axi4Params
    Nat -- addr width (in bits)
    Nat -- beat width (in bytes)
    Nat -- id

type family AddrWidth params where
  AddrWidth (Axi4Params a w i) = a

type family BeatWidth params where
  BeatWidth (Axi4Params a w i) = w

type family IdWidth params where
  IdWidth (Axi4Params a w i) = i

type KnownNatAxi4Params (p :: Axi4ParamsKind) =
  ( KnownNat (AddrWidth p)
  , KnownNat (BeatWidth p)
  , KnownNat (IdWidth p))

--data KnownNatAxi4Params p => RdRequest p =
--  RdRequest {
--    id :: Bit (IdWidth p),
--    addr :: Bit (AddrWidth p),
--    beat :: Bit (8 * BeatWidth p)
--  } deriving(Generic, Bits, Cmp)
