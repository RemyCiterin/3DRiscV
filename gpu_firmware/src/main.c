#include <stdint.h>
#include <stdbool.h>
#include "stdlib.h"

#define NCPU (2 * 16)

__attribute__((aligned(16))) char stack0[128 * NCPU];

inline int r_minstret() {
  int x;
  asm volatile("csrr %0, minstret" : "=r" (x));
  return x;
}

inline int r_mcycle() {
  int x;
  asm volatile("csrr %0, mcycle" : "=r" (x));
  return x;
}

static int volatile counter = 0;

static uint64_t volatile bitmask[NCPU];

static bool volatile wait = 0;

extern void main(int threadid) {
  if (threadid == 0) {
    for (int i=0; i < NCPU; i++) bitmask[i] = 0;
    wait = 1;
  }

  while (!wait) {}

  while (threadid && !bitmask[threadid-1]) {}

  int time = r_mcycle();
  int ins = r_minstret();
  printf("thread %d finish at %d cycles and %d instruction\n", threadid, time, ins);
  bitmask[threadid] = 1;
}
