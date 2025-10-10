.section .text.start
.global _start
_start:
  # a0 contains hartid
  # a1 contains device tree

  la t0, 0x10000004
  li t1, 0xAA
  sb t1, (t0)

  csrr t0, mhartid
  bnez t0, .wait_for_sync

  la t0, __bss_start
  la t1, __bss_end
  bgeu t0, t1, .bss_zero_loop_end
.bss_zero_loop:
  sb zero, (t0)
  addi t0, t0, 1
  bltu t0, t1, .bss_zero_loop
.bss_zero_loop_end:

  # Each thread view it's stack at the end of the address space
  addi sp, zero, -16

  la t0, _sync
  li t1, 1
  sw t1, (t0)

  csrr a0, mhartid
  jal main
.infinite_loop:
  j .infinite_loop

.wait_for_sync:
  la t0, _sync
  lw t0, (t0)
  bnez t0, .bss_zero_loop_end
  j .wait_for_sync



.section .data
_sync:
  .word 0
