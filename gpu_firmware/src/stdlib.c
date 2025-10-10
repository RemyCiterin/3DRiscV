#include "stdlib.h"

inline void simt_push() {
  asm volatile(".insn i CUSTOM_0, 0x0, zero, zero, 0\n");
}

inline void simt_pop() {
  asm volatile(".insn i CUSTOM_0, 0x1, zero, zero, 0\n");
}

inline void simt_sync() {
  simt_pop();
  simt_push();
}

