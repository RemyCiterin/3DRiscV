{-# Language FunctionalDependencies #-}
{-# Language UndecidableInstances #-}

module Utils where

import Blarney
import Blarney.Option
import Blarney.Core.BV

import Data.Proxy
import Blarney.TaggedUnion

-- -- like `whenTagged` but just lok at the tag
-- whenEnum :: (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m) =>
--   TagName name -> u -> Action a -> Action a
-- whenEnum tag u act = whenTagged tag u (\ _ -> act)
--
-- enum :: (IsTaggedUnion u, KnownSymbol name, Bit 0 ~ GetMemberType u name) =>
--   TagName name -> u
-- enum t = tag t 0

--  class (Enum a) => Represent a n | a -> n where
--  class (Represent a n, KnownNat n) => KnownRepresent a n where
--  instance (Represent a n, KnownNat n) => KnownRepresent a n where
--
--  class (Represent a n) => RepresentBuild a n where
--    type EnumW (n :: Nat) :: Nat
--
--  instance forall a n. (Represent a n) => RepresentBuild a n where
--    type EnumW n = n
--
--  --type EnumBit n = Bit (EnumW n)
--
--  newtype KnownRepresent a n => EnumBit a n = Bit n
--
--  enum :: forall t n. (KnownRepresent t n) => t -> Bit n
--  enum a = constant (toInteger (fromEnum a))

enum :: forall t n. (Enum t, KnownNat n) => t -> Bit n
enum a = constant (toInteger (fromEnum a))

item :: forall name u.
       (IsTaggedUnion u, KnownSymbol name, () ~ GetMemberType u name)
    => TagName name -> u
item x = tag x ()

infix 8 `is`
class HasIs a b where
  is :: a -> b -> Bit 1

--instance forall t n. (KnownRepresent t n) => HasIs (Bit n) t where
--  is bit value = bit === enum value

--infix 7 `is`
--is :: forall t. (KnownRepresent t) => Bit (EnumW t) -> t -> Bit 1
--is bit value = bit === enum value

-- like select but with a default value
selectDefault :: (Bits a) => a -> [(Bit 1, a)] -> a
selectDefault dft list =
  find ? (select list, dft)
    where
      find = orList (fmap fst list)
