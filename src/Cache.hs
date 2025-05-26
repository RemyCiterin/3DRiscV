module Cache where

import Ehr
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.ClientServer
import Blarney.SourceSink
import Arbiter
import Utils

-- address of a cache line
type Addr = Bit 32

-- key of a cache line
type Key = Bit 20

-- index of a cache line
type Index = Bit 6

-- offset of a word in a cache block
type Offset = Bit 4

-- Number of beats in a cache line
type LineBeats = 16

data LineState = LineState {
    dirty :: Bit 1,
    valid :: Bit 1,
    key :: Key
  } deriving(Generic, Bits, Interface)

-- Slave interface of the cache
data CpuRequest = CpuRequest {
    addr :: Addr,
    mask :: Bit 4,
    read :: Bit 1,
    beat :: Bit 32
  } deriving(Generic,Interface,Bits)

data CpuResponse = CpuResponse {
    beat :: Bit 32
  } deriving(Bits,Generic,Interface)

-- Master interface of the cache
-- Memory request
data MemRequest = MemRequest {
    read :: Bit 1, -- is the access a read
    addr :: Addr,  -- address of the access
    size :: Bit 4, -- size of the access in number of beat
    mask :: Bit 4, -- mask of the current beat in case of a write
    beat :: Bit 32,-- data of the current beat in case of a write
    last :: Bit 1  -- is the current beat the last of the burst
  } deriving(Generic,Interface,Bits)

-- Memory response (for read and write requests)
data MemResponse = MemResponse {
    beat :: Bit 32,-- data of the current beat in case of a write
    last :: Bit 1  -- is the current beat the last of the burst
  } deriving(Generic,Interface,Bits)

data AcquireIfc = AcquireIfc {
    canStart :: Bit 1,
    startBurst :: Addr -> Bit 10 -> Action (),
    canAck :: Bit 1,
    ack :: Action ()
  }

data AcquireStateTag
  = Idle
  | ReadWait -- Wait for a read response
  | ReadBurst -- Read burst
  deriving(Bounded, Enum)

instance Represent AcquireStateTag where
  type EnumW AcquireStateTag = 5

type AcquireState = EnumBit AcquireStateTag

makeAcquireFSM ::
  ArbiterClient -> RAM (Bit 10) (Bit 32) ->
  Module (AcquireIfc, Client MemRequest MemResponse)

makeAcquireFSM arbiter ram = do
  position :: Reg (Bit 10) <- makeReg dontCare
  state :: Reg AcquireState <- makeReg (enum Idle)


  return (AcquireIfc{
    canStart = 1,
    startBurst = \ addr pos -> pure (),
    canAck = 1,
    ack = pure ()
  }, Client{
    reqs = Source{
      canPeek = 1,
      peek = dontCare,
      consume = pure ()
    },
    resps = Sink{
      canPut = 1,
      put = \ req -> pure ()
    }
  })


--makeCache :: Module (Client CpuRequest CpuResponse, Server MemRequest MemResponse)
--makeCache = do
--  tagMem :: RAM Index LineState <- makeDualRAMForward
--  dataMem :: RAM (Bit 10) (Bit 32) <- makeDualRAMForward

--makeDualRAMForward
