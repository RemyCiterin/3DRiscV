module Cache where

import Blarney
import Blarney.Ehr
import Blarney.Queue
import Blarney.Option
import Blarney.ClientServer
import Blarney.SourceSink
import Blarney.Stream
import Blarney.Stmt
import Blarney.Arbiter
import Utils

-- address of a cache line
type Addr = Bit 32

-- key of a cache line
type Key = Bit 20

-- index of a cache line
type Index = Bit 6

-- Logarithm of the number of beat in a cache block
type BlockLogSize = 4

-- Number of beat in a cache block
type BlockSize = 2^BlockLogSize

-- offset of a word in a cache block
type Offset = Bit BlockLogSize

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
    read :: Bit 1,            -- is the access a read
    addr :: Addr,             -- address of the access
    mask :: Bit 4,            -- mask of the current beat in case of a write
    beat :: Bit 32,           -- data of the current beat in case of a write
    last :: Bit 1             -- is the current beat the last of the burst
  } deriving(Generic,Interface,Bits)

-- Memory response (for read and write requests)
data MemResponse = MemResponse {
    beat :: Bit 32,-- data of the current beat in case of a write
    last :: Bit 1  -- is the current beat the last of the burst
  } deriving(Generic,Interface,Bits)

data BurstIfc = BurstIfc {
    canStart :: Bit 1,
    startBurst :: Addr -> Bit 10 -> Action (),
    canAck :: Bit 1,
    ack :: Action ()
  }

-- Perform acquire (read) burst requests, this implementation doesn't
-- use an arbiter, so it must be used in blocking caches only
makeAcquireFSM ::
  RAMBE 10 4 -> Sink MemRequest -> Source MemResponse ->
  Module BurstIfc

makeAcquireFSM ram request response = do
  position :: Reg (Bit 10) <- makeReg dontCare
  addr :: Reg (Bit 32) <- makeReg dontCare

  length :: Reg (Bit BlockLogSize) <- makeReg 0

  busy :: Reg (Bit 1) <- makeReg false

  ready :: Wire (Bit 1) <- makeWire false
  startSignal :: Wire (Bit 1) <- makeWire false

  canAck :: Wire (Bit 1) <- makeWire false
  ack :: Wire (Bit 1) <- makeWire false

  runStmt do
    while true do
      -- Wait for a burst request
      while (inv busy.val) do
        action do
          ready <== true
          when (startSignal.val) do
            length <== ones
            busy <== true

      -- Wait for the response queue to be ready
      wait request.canPut

      -- Send memory request
      action do
        request.put MemRequest {
          beat= dontCare,
          mask= dontCare,
          addr= addr.val,
          last= true,
          read= true
        }

      -- Burst sequence
      while (length.val =!= 0) do
        wait response.canPeek
        action do
          response.consume
          ram.storeBE position.val ones response.peek.beat
          position <== position.val + 1
          length <== length.val - 1

      -- Send ack
      while (busy.val) do
        action do
          canAck <== true
          when (ack.val) do
            busy <== false

  return BurstIfc{
    canStart = ready.val,
    startBurst = \ a p -> do
      startSignal <== 1
      position <== p
      addr <== a,
    canAck = canAck.val,
    ack = ack <== 1
  }


-- Perform release (write) burst requests, this implementation doesn't
-- use an arbiter so it must be used in blocking caches only
makeReleaseFSM ::
  RAMBE 10 4 -> Sink MemRequest -> Source MemResponse ->
  Module BurstIfc

makeReleaseFSM ram request response = do
  nextPos :: Reg (Bit 10) <- makeReg dontCare
  addr :: Reg (Bit 32) <- makeReg dontCare

  length :: Reg (Bit BlockLogSize) <- makeReg 0

  busy :: Reg (Bit 1) <- makeReg false

  ready :: Wire (Bit 1) <- makeWire false
  startSignal :: Wire (Bit 1) <- makeWire false

  canAck :: Wire (Bit 1) <- makeWire false
  ack :: Wire (Bit 1) <- makeWire false

  runStmt do
    while true do
      -- Wait for a burst request
      while (inv busy.val) do
        action do
          ready <== true
          when (startSignal.val) do
            length <== ones
            busy <== true

      action do
        ram.loadBE nextPos.val
        nextPos <== nextPos.val + 1

      while (length.val =!= 0) do
        wait request.canPut
        action do
          ram.loadBE nextPos.val
          length <== length.val - 1
          nextPos <== nextPos.val + 1
          request.put MemRequest {
            last= length.val === 1,
            beat= ram.outBE,
            addr= addr.val,
            read= false,
            mask= ones
          }

      action do
        when response.canPeek do
          response.consume

      -- Send ack
      while busy.val do
        action do
            canAck <== true
            when (ack.val) do
              busy <== false

  return BurstIfc{
    canStart = ready.val,
    startBurst = \ a p -> do
      startSignal <== 1
      nextPos <== p
      addr <== a,
    canAck = canAck.val,
    ack = ack <== 1
  }

-- Initialize a block ram, and return if the ram owner can start it's procedure
initialiseRAM :: KnownNat aw => RAM (Bit aw) d -> d -> Module (Bit 1)
initialiseRAM ram init = do
  index :: Reg (Bit aw) <- makeReg 0
  start :: Reg (Bit 1) <- makeReg false

  runStmt do
    while (inv start.val) do
      action do
        index <== index.val + 1
        ram.store index.val init
        start <== index.val === ones

  return start.val

makeCache :: Stream CpuRequest -> Module (Stream CpuResponse, Client MemRequest MemResponse)
makeCache inputs = do
  tagMem :: RAM Index LineState <- makeDualRAMForward
  dataMem :: RAMBE 10 4 <- makeDualRAMForwardBE

  started <- initialiseRAM tagMem LineState{dirty= false, valid= false, key= 0}

  mustRead :: Reg (Bit 1) <- makeDReg false
  outputs :: Queue CpuResponse <- makeQueue

  busy :: Ehr (Bit 1) <- makeEhr 2 false
  request :: Reg CpuRequest <- makeReg dontCare

  memResponseQ :: Queue MemResponse <- makeQueue
  memRequestQ :: Queue MemRequest <- makeQueue

  acquireM <- makeAcquireFSM dataMem (toSink memRequestQ) (toSource memResponseQ)
  releaseM <- makeReleaseFSM dataMem (toSink memRequestQ) (toSource memResponseQ)

  acquire :: Reg (Bit 1) <- makeReg false
  release :: Reg (Bit 1) <- makeReg false

  always do
    -- Lookup stage
    when (inv (busy.read 1) .&&. inputs.canPeek .&&. started) do
      let index :: Index = slice @11 @6 inputs.peek.addr
      display "start lookup at address 0x" (formatHex 8 inputs.peek.addr)
      request <== inputs.peek
      busy.write 1 true
      inputs.consume

      tagMem.load index

    -- matching stage
    when (busy.read 0 .&&. inv acquire.val .&&. inv release.val .&&. outputs.notFull) do
      let key :: Key = slice @31 @12 request.val.addr
      let index :: Index = slice @11 @6 request.val.addr
      let offset :: Offset = slice @5 @2 request.val.addr

      if (tagMem.out.valid .&&. tagMem.out.key === key) then do
        -- Hit
        if (request.val.read) then do
          dataMem.loadBE (index # offset)
          busy.write 0 false
          mustRead <== true
        else do
          dataMem.storeBE (index # offset) request.val.mask request.val.beat
          tagMem.store index tagMem.out{dirty=true}
          busy.write 0 false
      else do
        -- Mis
        if (tagMem.out.dirty) then do
          tagMem.store index tagMem.out{key= key, valid= true, dirty=inv request.val.read}
          releaseM.startBurst (tagMem.out.key # index # 0) (index # 0)
          release <== true
        else do
          tagMem.store index tagMem.out{key= key, valid= true, dirty=inv request.val.read}
          acquireM.startBurst (key # index # 0) (index # 0)
          acquire <== true

    when (release.val .&&. releaseM.canAck) do
      let key :: Key = slice @31 @12 request.val.addr
      let index :: Index = slice @11 @6 request.val.addr

      release <== false
      releaseM.ack

      acquireM.startBurst (key # index # 0) (index # 0)
      acquire <== true

    when (acquire.val .&&. acquireM.canAck) do
      let key :: Key = slice @31 @12 request.val.addr
      let index :: Index = slice @11 @6 request.val.addr
      let offset :: Offset = slice @5 @2 request.val.addr

      acquire <== false
      acquireM.ack

      if (request.val.read) then do
        dataMem.loadBE (index # offset)
        busy.write 0 false
        mustRead <== true
      else do
        dataMem.storeBE (index # offset) request.val.mask request.val.beat
        busy.write 0 false

    when mustRead.val do
      outputs.enq CpuResponse{beat= dataMem.outBE}

  return (toStream outputs, Client {reqs= toSource memRequestQ, resps= toSink memResponseQ})

testCache :: Module ()
testCache = do
  queue :: Queue CpuRequest <- makeQueue

  cycle :: Reg (Bit 32) <- makeReg 0

  (resp, client) <- makeCache (toStream queue)

  length :: Reg (Bit 4) <- makeReg 0

  always do
    when (client.reqs.canPeek) do
      if (client.reqs.peek.read) then do
        display "read address: " client.reqs.peek.addr
      else do
        display "write address: " client.reqs.peek.addr " data: " client.reqs.peek.beat
      client.reqs.consume

    when (client.resps.canPut) do
      client.resps.put MemResponse{beat= 0,last=length.val === ones}
      length <== length.val + 1

  runStmt do
    wait queue.notFull
    action do
      queue.enq CpuRequest { addr= 0, beat= 0, mask= 0, read= true }

    wait resp.canPeek
    action do
      display "response: " resp.peek.beat
      resp.consume

    action do
      queue.enq CpuRequest { addr= 0, beat= 42, mask= 0b1111, read= false }
    action do
      queue.enq CpuRequest { addr= 4, beat= 43, mask= 0b1111, read= false }
    action do
      queue.enq CpuRequest { addr= 16*4, beat= 57, mask= 0b1111, read= false }

    action do
      queue.enq CpuRequest { addr= 0, beat= 0, mask= 0, read= true }

    wait resp.canPeek
    action do
      display "response: " resp.peek.beat
      resp.consume

    action do
      queue.enq CpuRequest { addr= 0x80000000, beat= 42, mask= 0b1111, read= false }

    action do
      queue.enq CpuRequest { addr= 0x80000000, beat= 0, mask= 0, read= true }

    wait resp.canPeek
    action do
      display "response: " resp.peek.beat
      resp.consume

  return ()
