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


readOnlyReg :: forall n. KnownNat n => Bit n -> Reg (Bit n)
readOnlyReg x =
  Reg
    { readReg= x
    , writeReg= \ _ -> return () }

lowerReg :: forall m n. (KnownNat m, KnownNat n, m <= n) => Reg (Bit n) -> Reg (Bit m)
lowerReg r =
  Reg
    { readReg= FromBV $ lower (toBV r.val)
    , writeReg= \ x -> do
        r <== FromBV (concatBV (upper (toBV r.val)) (toBV x))}
  where
    upper = selectBV (wn-1,wm)
    lower = selectBV (wm-1,0)
    wm = valueOf @m
    wn = valueOf @n

upperReg :: forall m n. (KnownNat m, KnownNat n, m <= n) => Reg (Bit n) -> Reg (Bit m)
upperReg r =
  Reg
    { readReg= FromBV $ upper (toBV r.val)
    , writeReg= \ x -> do
        r <== FromBV (concatBV (toBV x) (lower (toBV r.val)))}
  where
    upper = selectBV (wn-1,wm)
    lower = selectBV (wm-1,0)
    wm = valueOf @m
    wn = valueOf @n

infixr 6 `concatReg`
concatReg :: forall n m.
  (KnownNat n, KnownNat m, KnownNat (n+m))
    => Reg (Bit n)
    -> Reg (Bit m)
    -> Reg (Bit (n+m))
concatReg lhs rhs =
  Reg
    { readReg= lhs.val # rhs.val
    , writeReg= \x -> do
        lhs <== truncateLSBCast x
        rhs <== truncateCast x }
