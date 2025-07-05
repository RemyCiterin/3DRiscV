module Blarney.ADT
  ( module Blarney.TaggedUnion
  , HasIs(..)
  , enum
  , item
  ) where

import Blarney
import Blarney.TaggedUnion hiding(is)

enum :: forall t n. (Enum t, KnownNat n) => t -> Bit n
enum a = constant (toInteger (fromEnum a))

-- A tag with a unit type
item :: forall name u.
       (IsTaggedUnion u, KnownSymbol name, () ~ GetMemberType u name)
    => TagName name -> u
item x = tag x ()

infix 8 `is`
class HasIs a b where
  is :: a -> b -> Bit 1

-- Sadly none of those tentatives works

instance (IsTaggedUnion u, KnownSymbol name) => HasIs u (TagName name) where
  is = isTagged

--instance (IsTaggedUnion u, KnownSymbol name) => HasField "is" u (TagName name) where
--  getField = isTagged
