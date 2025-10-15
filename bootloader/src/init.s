.section .text.start
.global _start
_start:
  # a0 contains hartid
  # a1 contains device tree

  la t0, 0x10000004
  li t1, 0xAA
  sb t1, (t0)

  csrr t0, mhartid
  bnez t0, .gpu_start

  la t0, __bss_start
  la t1, __bss_end
  bgeu t0, t1, .bss_zero_loop_end
.bss_zero_loop:
  sb zero, (t0)
  addi t0, t0, 1
  bltu t0, t1, .bss_zero_loop
.bss_zero_loop_end:

  // Load the SDCARD content in SDRAM at address 0x80010000
  la sp, stack_top
  jal bootloader_main

  // Synchronyze all the other threads
  la t0, sync
  li t1, 1
  sw t1, (t0)

.jump_to_sdram:
  li a0, 0x80010000

  // Ensure eveything we loaded is visible from the I-cache
  fence
  fence.i
  // Jump to the SDRAM at address 0x80010000
  jalr a0

.infinite_loop:
  j .infinite_loop

.gpu_start:
  la t0, sync
  lw t1, (t0)
  bnez t1, .jump_to_sdram
  j .gpu_start


.section .bss
.align 4
  .skip 0x2000
stack_top:
  .skip 0x100

.section .data
.align 4
sync:
  .word 0
