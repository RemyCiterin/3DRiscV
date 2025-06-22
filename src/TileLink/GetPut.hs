module TileLink.GetPut where

import Blarney
import Blarney.Queue
import Blarney.SourceSink
import Blarney.ADT
import Blarney.Connectable
import Blarney.Arbiter
import Blarney.Stmt

import TileLink.Interconnect
import TileLink.Utils
import TileLink.Types
import TileLink.RAM

data GetMaster iw (p :: TLParamsKind) =
  GetMaster
    { canGet :: Bit 1
    , get :: Bit iw -> Bit (AddrWidth p) -> Bit (SizeWidth p) -> Action ()
    , canGetAck :: Bit 1
    , getAck :: Action ()
    , active :: Bit 1
    , size :: Bit (SizeWidth p)
    , address :: Bit (AddrWidth p)
    , index :: Bit iw }

makeGetMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> TLSlave p
    -> Module (GetMaster iw p)
makeGetMaster source arbiter ram slave = do
  metaD <- makeMetaSourceD @p slave.channelD
  let channelD = metaD.source

  mask :: Reg (Bit (LaneWidth p)) <- makeReg dontCare
  index :: Reg (Bit iw) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false
  last :: Reg (Bit 1) <- makeReg false

  request :: Reg (Bit iw, Bit (AddrWidth p), Bit (SizeWidth p)) <- makeReg dontCare

  always do
    when (channelD.canPeek) do
      let msg = channelD.peek
      when (msg.opcode `isTagged` #AccessAckData .&&. msg.source === source) do
        arbiter.request

      when (arbiter.grant) do
        channelD.consume

        ram.storeBE index.val mask.val msg.lane
        index <== index.val + 1
        last <== metaD.last

  return GetMaster
    { get= \ idx addr logSize -> do
        dynamicAssert (addr .&. ((1 .<<. logSize) - 1) === 0) "unaligned address"
        slave.channelA.put
          ChannelA
            { opcode= tag #Get ()
            , lane= dontCare
            , mask= getLaneMask @p addr logSize
            , address= addr
            , size= logSize
            , source= source}
        mask <== getLaneMask @p addr logSize
        request <== (idx,addr,logSize)
        valid <== true
        index <== idx
    , getAck= do
        valid <== false
        last <== false
    , canGet= slave.channelA.canPut .&&. inv valid.val
    , canGetAck= last.val
    , active= valid.val
    , address= let (_,b,_) = request.val in b
    , index= let (a,_,_) = request.val in a
    , size= let (_,_,c) = request.val in c }

data PutMaster iw (p :: TLParamsKind) =
  PutMaster
    { canPut :: Bit 1
    , put :: Bit iw -> Bit (AddrWidth p) -> Bit (SizeWidth p) -> Action ()
    , canPutAck :: Bit 1
    , putAck :: Action ()
    , active :: Bit 1
    , address :: Bit (AddrWidth p)
    , size :: Bit (SizeWidth p)
    , index :: Bit iw }

makePutMaster ::
  forall iw p. (KnownNat iw, KnownTLParams p)
    => Bit (SourceWidth p)
    -> ArbiterClient
    -> RAMBE iw (LaneWidth p)
    -> TLSlave p
    -> Module (PutMaster iw p)
makePutMaster source arbiter ram slave = do
  let laneSize :: TLSize = constant $ toInteger $ valueOf @(LaneWidth p)
  metaD <- makeMetaSourceD @p slave.channelD
  let channelD = metaD.source

  queue :: Queue () <- makeQueue
  message :: Reg (ChannelA p) <- makeReg dontCare
  valid :: Reg (Bit 1) <- makeReg false

  size :: Reg TLSize <- makeReg 0
  index :: Reg (Bit iw) <- makeReg dontCare

  request :: Reg (Bit iw, Bit (AddrWidth p), Bit (SizeWidth p)) <- makeReg dontCare

  always do
    when (size.val =!= 0 .&&. queue.notFull) do
      arbiter.request
    when (arbiter.grant) do
      size <== size.val .>=. laneSize ? (size.val - laneSize, 0)
      index <== index.val + 1

      ram.loadBE index.val
      queue.enq ()

    when (queue.canDeq .&&. slave.channelA.canPut) do
      slave.channelA.put (message.val { lane= ram.outBE } :: ChannelA p)
      queue.deq

  return PutMaster
    { put= \ idx addr logSize -> do
        dynamicAssert (addr .&. ((1 .<<. logSize) - 1) === 0) "unaligned address"
        message <==
          ChannelA
            { opcode= tag #PutData ()
            , lane= dontCare
            , mask= getLaneMask @p addr logSize
            , address= addr
            , source= source
            , size= logSize}
        request <== (idx,addr,logSize)
        size <== 1 .<<. logSize
        valid <== true
        index <== idx
    , putAck= do
        channelD.consume
        valid <== false
    , canPut= inv valid.val .&&. inv queue.canDeq
    , canPutAck=
        channelD.canPeek .&&. channelD.peek.opcode `isTagged` #AccessAck .&&.
        channelD.peek.source === source .&&. size.val === 0
    , active= valid.val
    , address= let (_,b,_) = request.val in b
    , index= let (a,_,_) = request.val in a
    , size= let (_,_,c) = request.val in c}

--  makeTestGetPut :: (Bit 1) -> Module (Bit 1)
--  makeTestGetPut _ = do
--    let config =
--          TLRAMConfig
--            { lowerBound= 0x80000000
--            , bypassChannelA= False
--            , bypassChannelD= False
--            , sink= 0
--            , fileName= Just "Mem.hex" }
--    slave <- makeTLRAM @16 @(TLParams 32 4 4 8 8) config
--
--    let xbarconfig =
--          XBarConfig
--            { bce= True
--            , rootAddr= \ _ -> 0
--            , rootSink= \ _ -> 0
--            , rootSource= \ x -> x === 0 ? (0,1)
--            , sizeChannelA= 2
--            , sizeChannelB= 2
--            , sizeChannelC= 2
--            , sizeChannelD= 2
--            , sizeChannelE= 2}
--
--    ([master0], [slave0,slave1]) <- makeTLXBar @1 @2 @(TLParams 32 4 4 8 8) xbarconfig
--
--    makeConnection master0 slave
--
--    ram :: RAMBE 10 4 <- makeDualRAMBE
--
--    getArbiter <- makeNullArbiter
--    putArbiter <- makeNullArbiter
--
--    getM <- makeGetMaster 0 getArbiter ram slave0
--    putM <- makePutMaster 1 putArbiter ram slave1
--
--    index :: Reg (Bit 10) <- makeReg 0
--    queue :: Queue (Bit 10) <- makePipelineQueue 1
--
--    always do
--      when (queue.canDeq) do
--        let idx = queue.first
--        display "index: " idx " value: 0x" (formatHex 8 ram.outBE)
--        queue.deq
--
--    runStmt do
--      action do
--        ram.storeBE 0 0b0100 0x00FF0000
--      action do
--        ram.storeBE 5 0b1111 0x12345678
--      action do
--        ram.storeBE 6 0b1111 0x87654321
--      action do
--        ram.loadBE index.val
--        queue.enq index.val
--      wait putM.canPut
--      action do
--        putM.put 0 0x80000002 1
--      wait putM.canPutAck
--      action do
--        putM.putAck
--
--      wait putM.canPut
--      action do
--        putM.put 5 0x80000008 3
--      wait putM.canPutAck
--      action do
--        putM.putAck
--
--      wait getM.canGet
--      action do
--        display "start get sequence"
--        getM.get 0 0x80000000 4
--
--      wait getM.canGetAck
--      action do
--        getM.getAck
--
--      while (index.val .<. 16) do
--        wait queue.notFull
--        action do
--          index <== index.val + 1
--          ram.loadBE index.val
--          queue.enq index.val
--
--    -- ensure the circuit is not remove by yosys/nextpnr
--    return master0.channelA.canPeek
