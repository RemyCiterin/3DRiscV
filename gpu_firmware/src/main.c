#include <stdint.h>
#include <stdbool.h>
#include "stdlib.h"
#include "stats.h"

#define NCPU (4 * 16)

__attribute__((aligned(16))) char stack0[128 * NCPU];

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

static int volatile counter = 0;

static uint64_t volatile bitmask[NCPU];

static bool volatile wait = 0;

static int m1[NCPU][NCPU];
static int m2[NCPU][NCPU];
static int m3[NCPU][NCPU];

extern void main(int threadid) {
  // Initialize thread locks
  if (threadid == 0) {
    for (int i=0; i < NCPU; i ++) {
      for (int j=0; j < NCPU; j++) {
        m1[i][j] = i;
      }
    }
    for (int i=0; i < NCPU; i ++) {
      for (int j=0; j < NCPU; j++) {
        m2[i][j] = i;
      }
    }

    for (int i=0; i < NCPU; i++) bitmask[i] = 0;
    wait = 1;
  }
  while (!wait) {}

  simt_sync();
  timestamp_t timestamp;
  init_timestamp(&timestamp);
  simt_sync();

  for (int i=0; i < NCPU; i++) {
    for (int j=0; j < NCPU; j++) {
      m3[i][threadid] += m1[i][j] * m2[j][threadid];
    }
  }

  // Wait for the execution of all the previous threads to finish
  while (threadid && !bitmask[threadid-1]) {}

  // Print statistics
  print_stats(threadid, &timestamp);

  //for (int i=0; i < NCPU; i++) {
  //  printf("%d ", m3[i][threadid]);
  //}
  //printf("\n");

  // Allow the next thread to start
  bitmask[threadid] = 1;
}
