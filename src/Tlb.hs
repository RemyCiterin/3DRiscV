module Tlb where

import Instr
import Blarney
import Blarney.Utils
import Blarney.Queue
import Blarney.SourceSink

data VPN =
  VPN
    { offsetVPN :: Bit 12
    , vpn0 :: Bit 10
    , vpn1 :: Bit 10 }
  deriving(Generic, Bits)

-- Nodes:
--    - A supervisor software can access to a user PTE if SUM is set
data PTE =
  PTE
    { validPTE :: Bit 1     -- valid Page Table Entry
    , readPTE :: Bit 1      -- readable Page Table Entry
    , writePTE :: Bit 1     -- writable Page Table Entry
    , executePTE :: Bit 1   -- executable Page Table Entry
    , userPTE :: Bit 1      -- unser can access to this entry
    , globalPTE :: Bit 1    -- don't flush this entry from TLB
    , accessedPTE :: Bit 1  -- accessed bit (set at any access)
    , dirtyPTE :: Bit 1     -- dirty bit (set at anywrite)
    , rswPTE :: Bit 2       -- reserved for software use
    , ppn :: Bit 22 }       -- physical page number
  deriving(Generic, Bits)


