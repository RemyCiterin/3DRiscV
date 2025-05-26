module Ehr where

import Blarney
import Data.Proxy
import Blarney.Queue

data Ehr a =
  Ehr
    { read :: Int -> a
    , write :: Int -> a -> Action ()}

makeEhr :: (Bits a) => Int -> a -> Module (Ehr a)
makeEhr number init = do
  reg :: Reg a <- makeReg init
  wires :: [Wire a] <- replicateM number (makeWire dontCare)

  always do
    reg <== readAt reg wires number

  return Ehr {
    read = \i -> readAt reg wires i,
    write = \i a -> do
      wires !! i <== a
  }

  where
    readAt reg wires n
      | n == 0 = reg.val
      | otherwise =
        (wires !! (n-1)).active ?
          ((wires !! (n-1)).val, readAt reg wires (n-1))
