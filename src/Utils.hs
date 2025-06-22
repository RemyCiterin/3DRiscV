{-# Language FunctionalDependencies #-}
{-# Language UndecidableInstances #-}

module Utils where

import Blarney
import Blarney.Option
import Blarney.Core.BV

import Data.Proxy

-- like select but with a default value
selectDefault :: (Bits a) => a -> [(Bit 1, a)] -> a
selectDefault dft list =
  find ? (select list, dft)
    where
      find = orList (fmap fst list)
