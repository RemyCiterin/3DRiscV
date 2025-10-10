ENTRY(_start)

MEMORY
{
    imem (rx): ORIGIN = 0x80000000, LENGTH = 20K
    dmem (rw): ORIGIN = 0x80010000, LENGTH = 70K
}

SECTIONS
{
  .text : {
    KEEP(*(.text.start))
    *(.text*)
  } > imem

  .rodata : {
    *(.rodata .rodata.*)
  } > dmem

  PROVIDE(__bss_start = .);
  .sbss : ALIGN(16) {
    *(.sbss .sbss.*)
  } > dmem

  .bss : ALIGN(16) {
    *(.bss .bss.*)
  } > dmem
  PROVIDE(__bss_end = .);

  .data : ALIGN(16) {
      *(.data .data.*)
  } > dmem

  . = ALIGN(4k);
  PROVIDE(stack0 = .);
}
