void printf(const char*, ...);
//unsigned strlen(const char*);
//void* memset(void*, int, unsigned);
//char* strcpy(char*, const char*);

//void simt_pop();
//void simt_push();
//void simt_sync();

inline __attribute__((always_inline)) void simt_push() {
  asm volatile(".insn i CUSTOM_0, 0x0, zero, zero, 0\n");
}

inline __attribute__((always_inline)) void simt_pop() {
  asm volatile(".insn i CUSTOM_0, 0x1, zero, zero, 0\n");
}

inline __attribute__((always_inline)) void simt_sync() {
  simt_pop();
  simt_push();
}

