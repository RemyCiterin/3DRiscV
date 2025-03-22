{-# Language FunctionalDependencies #-}
{-# Language UndecidableInstances #-}

module Utils where

import Blarney
import Blarney.Option
import Blarney.Core.BV

import Data.Proxy

-- -- like `whenTagged` but just lok at the tag
-- whenEnum :: (IsTaggedUnion u, KnownSymbol name, m ~ GetMemberType u name, Bits m) =>
--   TagName name -> u -> Action a -> Action a
-- whenEnum tag u act = whenTagged tag u (\ _ -> act)
--
-- enum :: (IsTaggedUnion u, KnownSymbol name, Bit 0 ~ GetMemberType u name) =>
--   TagName name -> u
-- enum t = tag t 0

class (Enum a) => Represent a where
  type EnumW a :: Nat
class (Represent a, KnownNat (EnumW a)) => KnownRepresent a where
instance (Represent a, KnownNat (EnumW a)) => KnownRepresent a where


type EnumBit a = Bit (EnumW a)

-- infix 7 `is`
-- is :: forall t. (KnownRepresent t) => Bit (EnumW t) -> t -> Bit 1
-- is bit value = bit === enum value

enum :: forall t. (KnownRepresent t) => t -> Bit (EnumW t)
enum a = constant (toInteger (fromEnum a))

-- like select but with a default value
selectDefault :: (Bits a) => a -> [(Bit 1, a)] -> a
selectDefault dft list =
  find ? (select list, dft)
    where
      find = orList (fmap fst list)
