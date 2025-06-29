module Blarney.Utils where

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

toByteList :: forall n. (KnownNat n) => Bit (8*n) -> [Bit 8]
toByteList bytes = go 0
  where
    go :: Int -> [Bit 8]
    go x
      | x == valueOf @n = []
      | otherwise = unsafeSlice (8*x+7,8*x) bytes : go (x+1)

fromByteList :: forall n. (KnownNat n) => [Bit 8] -> Bit (8 * n)
fromByteList bs = FromBV $ foldr1 concatBV (reverse (fmap toBV bs))

mergeBE :: forall n.
  (KnownNat n, KnownNat (8*n))
    => Bit (8*n)
    -> Bit (8*n)
    -> Bit n
    -> Bit (8*n)
mergeBE new old mask =
  fromByteList (go (toByteList new) (toByteList old) (toBitList mask))
  where
    go (x:xs) (y:ys) (m:ms) =
      (m ? (x,y)) : go xs ys ms
    go [] [] [] = []
