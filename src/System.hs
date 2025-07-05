module System where

import Blarney
import Blarney.SourceSink
import Blarney.Option
import Blarney.Utils
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
    , canInterrupt :: Option CauseInterrupt
    , satp :: Bit 32
    , mxr :: Bit 1
    , sum :: Bit 1
    , priv :: Priv }

makeSystem ::
  Integer
  -> SystemInputs
  -> Module SystemIfc
makeSystem hartId inputs = do
  let misaCSRs = [readOnlyCSR 0xf10 0b01000000000101000000000100000001]
  let mvendoridCSRs = [readOnlyCSR 0xf11 0]
  let marchidCSRs = [readOnlyCSR 0xf12 0]
  let mimpidCSRs = [readOnlyCSR 0xf13 0]

  (mieCSRs, mie) <- makeInterruptEnableCSRs
  (mipCSRs, mip) <- makeInterruptPendingCSRs
  (delegCSRs, deleg) <- makeDelegCSRs
  cycleCRSs <- makeCycleCounterCSR
  (statusCSRs, status) <- makeStatusCSRs
  hartIdCSRs <- makeHartIdCSR (constant hartId)
  (instretCSRs, instret) <- makeInstructionCounterCSR
  (satpCSRs, satp) <- makeSatpCSRs
  (trapCSRs, trap) <- makeTrapCSRs
  mscratchCSRs <- makeMscratchCSRs
  sscratchCSRs <- makeSscratchCSRs

  let priv = status.priv

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
      ++ sscratchCSRs
      ++ marchidCSRs
      ++ statusCSRs
      ++ mimpidCSRs
      ++ delegCSRs
      ++ misaCSRs
      ++ satpCSRs
      ++ trapCSRs
      ++ mieCSRs
      ++ mipCSRs

  let canInterrupt :: Option CauseInterrupt = -- DONE
        let ready :: Bit 16 = lower (mip.all .&. mie.all) in
        -- Machine mode
        let machine_mask :: Bit 16 =
              ready .&. inv (lower deleg.mideleg.val) in
        let machine_enabled :: Bit 1 =
              status.mie.val .||. (priv.val .<. machine_priv) in
        -- Supervisor mode
        let supervisor_mask :: Bit 16 =
              ready .&. lower deleg.mideleg.val in
        let supervisor_enabled :: Bit 1 =
              (status.sie.val .&&. priv.val === supervisor_priv) .||. priv.val .<. supervisor_priv in

        let mask :: Bit 16 =
              (machine_enabled ? (machine_mask, 0))
              .|. (supervisor_enabled ? (supervisor_mask, 0))
        in

        if mask =!= 0 then
          some $ unpack $ binaryEncode (firstHot ready)
        else
          none

  let execException = \ epc interrupt cause tval -> do
        let toS :: Bit 1 =
              priv.val .<=. supervisor_priv .&&.
                lower ((interrupt ? (deleg.mideleg.val, deleg.medeleg.val)) .>>.cause)
        let newPriv = toS ? (supervisor_priv, machine_priv)
        let cause_msb = interrupt ? (2^31, 0)

        if toS then do
          trap.scause <== cause_msb .|. zeroExtend cause
          status.spp <== priv.val =!= user_priv
          status.spie <== status.sie.val
          status.sie <== false
          trap.stval <== tval
          trap.sepc <== epc
        else do
          trap.mcause <== cause_msb .|. zeroExtend cause
          status.mpie <== status.mie.val
          status.mpp <== pack priv.val
          status.mie <== false
          trap.mtval <== tval
          trap.mepc <== epc

        let tvec = toS ? (trap.stvec.val, trap.mtvec.val)
        let base = tvec .&. inv 0b11
        let vec = slice @1 @0 tvec === 0b01
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
                , flush= false
                , rd= dontCare }
          else if input.instr.opcode `is` [MRET] then do
            if priv.val === machine_priv then do
              let newPriv = Priv status.mpp.val
              status.mpie <== false
              priv <== newPriv
              status.mpp <== 0

              if newPriv === machine_priv then do
                status.mie <== status.mpie.val
              else do
                status.sie <== status.mpie.val
              display "mret to 0x" (formatHex 0 trap.mepc.val)
              return
                ExecOutput
                  { cause= dontCare
                  , exception= false
                  , pc= trap.mepc.val
                  , tval= dontCare
                  , flush= false
                  , rd= dontCare }
            else do
              return
                ExecOutput
                  { cause= illegal_instruction
                  , tval= input.instr.raw
                  , exception= true
                  , pc= dontCare
                  , flush= false
                  , rd= dontCare }
          else if input.instr.opcode `is` [SRET] then do
            if priv.val === supervisor_priv then do
              let newPriv =
                    status.spp.val === 1 ?
                    (supervisor_priv, user_priv)
              status.sie <== status.spie.val
              status.spie <== false
              priv <== newPriv
              status.spp <== 0

              display "sret to 0x" (formatHex 0 trap.mepc.val)
              return
                ExecOutput
                  { cause= dontCare
                  , exception= false
                  , pc= trap.mepc.val
                  , tval= dontCare
                  , flush= false
                  , rd= dontCare }
            else do
              return
                ExecOutput
                  { cause= illegal_instruction
                  , tval= input.instr.raw
                  , exception= true
                  , flush= false
                  , pc= dontCare
                  , rd= dontCare }
          else if input.instr.opcode `is` [WFI] then do
            return
              ExecOutput
                { cause= dontCare
                , exception= false
                , flush= false
                , pc=
                    (status.mie.val .&&. (mip.all .&. mie.all =!= 0)) ?
                      (input.pc + 4, input.pc)
                , tval= dontCare
                , rd= dontCare }
          else if input.instr.opcode `is` [ECALL] then do
            return
              ExecOutput
                { cause= ecall_from_m
                , exception= true
                , tval= input.pc
                , flush= false
                , pc= dontCare
                , rd= dontCare }
          else if input.instr.opcode `is` [FENCE_I] then do
            return
              ExecOutput
                { cause= dontCare
                , exception= false
                , tval= dontCare
                , flush= true
                , pc= dontCare
                , rd= dontCare }
          else do
            execCSR priv.val csrUnit input
      , exception= \ epc cause tval -> execException epc false (pack cause) tval
      , interrupt= \ epc cause tval -> execException epc true (pack cause) tval
      , mxr= status.mxr.val
      , sum= status.sum.val
      , satp= satp.val
      , priv= priv.val
      , canInterrupt
      , instret }
