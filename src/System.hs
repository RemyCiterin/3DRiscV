module System where

import Blarney
import Blarney.SourceSink
import Blarney.Option
import Blarney.Queue
import Blarney.ADT

import Instr
import CSR
import Alu

data SystemInputs =
  SystemInputs
    { softwareInterrupt :: Bit 1
    , timerInterrupt :: Bit 1
    , externalInterrupt :: Bit 1 }
  deriving(Generic, Bits, Interface)

-- only one procedure in `exec,exception,interrupt` can be used per cycle
-- except if `exec` fail (in this case you must call exception)
data SystemIfc =
  SystemIfc
    { exec :: ExecInput -> Action ExecOutput
    , instret :: Action ()
    , exception :: Bit 32 -> CauseException -> Bit 32 -> Action (Bit 32)
    , interrupt :: Bit 32 -> CauseInterrupt -> Bit 32 -> Action (Bit 32)
    , canInterrupt :: Option CauseInterrupt }

makeSystem ::
  Integer
  -> SystemInputs
  -> Module SystemIfc
makeSystem hartId inputs = do
  let priv :: Priv = machine_priv

  let mvendoridCSRs = [readOnlyCSR 0xf11 0]
  let marchidCSRs = [readOnlyCSR 0xf12 0]
  let mimpidCSRs = [readOnlyCSR 0xf13 0]

  (mieCSRs, mie) <- makeMieCSRs
  (mipCSRs, mip) <- makeMipCSRs
  cycleCRSs <- makeCycleCounterCSR
  (mstatusCSRs, mstatus) <- makeMstatusCSRs
  hartIdCSRs <- makeHartIdCSR (constant hartId)
  (instretCSRs, instret) <- makeInstructionCounterCSR
  (trapCSRs, trap) <- makeTrapCSRs
  mscratchCSRs <- makeMscratchCSRs

  always do
    mip.meip <== inputs.externalInterrupt
    mip.msip <== inputs.softwareInterrupt
    mip.mtip <== inputs.timerInterrupt

  csrUnit <-
    makeCSRUnit $
      cycleCRSs
      ++ instretCSRs
      ++ hartIdCSRs
      ++ mvendoridCSRs
      ++ mscratchCSRs
      ++ marchidCSRs
      ++ mstatusCSRs
      ++ mimpidCSRs
      ++ trapCSRs
      ++ mieCSRs
      ++ mipCSRs

  let execException = \ epc interrupt cause tval -> do
        trap.mepc <== epc
        let cause_msb = interrupt ? (2^31, 0)
        trap.mcause <== cause_msb .|. zeroExtend cause
        trap.mtval <== tval

        mstatus.mpie <== mstatus.mie.val
        mstatus.mie <== false

        let base = trap.mtvec.val .&. inv 0b11
        let vec = slice @1 @0 trap.mtvec.val === 0b01
        return $ (vec .&&. interrupt) ? (base + zeroExtend cause * 4, base)

  return
    SystemIfc
      { exec= \input -> do
          if input.instr.opcode === 0 then do
            return
              ExecOutput
                { cause=illegal_instruction
                , exception= true
                , tval=input.pc
                , pc= dontCare
                , rd= dontCare }
          else if input.instr.opcode `is` [MRET] then do
            mstatus.mie <== mstatus.mpie.val
            mstatus.mpie <== false
            display "mret to 0x" (formatHex 0 trap.mepc.val)
            return
              ExecOutput
                { cause= dontCare
                , exception= false
                , pc= trap.mepc.val
                , tval= dontCare
                , rd= dontCare }
          else if input.instr.opcode `is` [WFI] then do
            return
              ExecOutput
                { cause= dontCare
                , exception= false
                , pc=
                    (mstatus.mie.val .&&. (mip.all .&. mie.all =!= 0)) ?
                      (input.pc + 4, input.pc)
                , tval= dontCare
                , rd= dontCare }
          else if input.instr.opcode `is` [ECALL] then do
            return
              ExecOutput
                { cause= ecall_from_m
                , exception= true
                , tval= input.pc
                , pc= dontCare
                , rd= dontCare }
          else do
            execCSR priv csrUnit input
      , canInterrupt=
          let ready :: Bit 16 = lower mip.all .&. lower mie.all in
          let enabled = mstatus.mie.val in

          if enabled .&&. ready =!= 0 then
            some $ unpack $ binaryEncode (firstHot ready)
          else
            none
      , exception= \ epc cause tval -> execException epc false (pack cause) tval
      , interrupt= \ epc cause tval -> execException epc true (pack cause) tval
      , instret }
