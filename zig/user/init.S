.section .text.init
.global _reboot
.global _start



_start:
  la t0, bss_start
  la t1, bss_end
  bgeu t0, t1, .bss_zero_loop_end
.bss_zero_loop:
  sb zero, (t0)
  addi t0, t0, 1
  bltu t0, t1, .bss_zero_loop
.bss_zero_loop_end:

  la sp, stack_top0
  la t0, bss_init_finish
  li t1, 1
  sw t1, 0(t0)
  jal main
.infinite_loop:
  j .infinite_loop

.section .data

.section .data
.align 4
bss_init_finish:
  .word 0

.section .bss
.align 4
  .skip 0x1000
stack_top0:
  .skip 0x4
