module Plic where

import Blarney
import Blarney.Option
import Blarney.Queue

import TileLink
import TileLink.Mmio

import Data.Proxy

data PlicEntry =
  PlicEntry
    { plicId :: Bit 10
    , plicPending :: Bit 1 }
  deriving(Generic, Bits, Interface)

makePlic :: forall p.
  ( KnownTLParams p
  , 32 ~ AddrWidth p
  , 4 ~ LaneWidth p )
    => Int
    -> [PlicEntry]
    -> Bit (AddrWidth p)
    -> Module [Mmio p]
makePlic numHart entries base =
  liftNat (log2 (length entries)) $ \ (_ :: Proxy width) -> do

    priorities :: [Reg (Bit 32)] <-
      replicateM (length entries) (makeReg 0)

    -- Say for each pair hart/entry if the interrupt has been claimed
    -- but not completed by the hart
    claimed :: [[Reg (Bit 1)]] <-
      replicateM numHart $ replicateM (length entries) $ makeReg false

    -- Say if an interrupt is pending for a given core
    let pending :: [[Bit 1]] =
          [
            [ entry.plicPending .&&. inv c.val
            | (entry,c) <- zip entries cs ]
          | cs <- claimed]

    -- Priority threshold for each core
    threshold :: [Reg (Bit 32)] <-
      replicateM numHart $ makeReg 0

    enabled :: [[Reg (Bit 1)]] <-
      replicateM numHart $ replicateM (length entries) $ makeReg false

    let highestPendingPriority :: Int -> Bit width = \ hart ->
          let go = \ (pending1, prio1) (pending2, prio2) ->
                select
                  [ inv pending2 --> (pending1, prio1)
                  , pending2 .&&. inv pending1 --> (pending2, prio2)
                  , pending1 .&&. pending2 .&&. prio1 .>. prio2 --> (pending2, prio2)
                  , pending1 .&&. pending2 .&&. prio2 .>. prio1 --> (pending1, prio1) ]
          in

          let (p, (prio, index)) = tree go (false, (0, 0)) $
                zip (pending!hart) $ zip [-p.val | p <- priorities] $ map lit [0..] in

          (p .&&. prio .>. (threshold!hart).val) ? (index, 0)

    let claim :: Int -> Bit width -> Action () = \hart id -> do
          (claimed!hart)!id <== true

    let complete :: Int -> Bit width -> Action () = \hart id -> do
          (claimed!hart)!id <== false

    return []






