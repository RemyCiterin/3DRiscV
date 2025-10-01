#include <stdint.h>
#include <stdbool.h>
#include "stdlib.h"
#include "stats.h"

#define NCPU (4 * 16)

__attribute__((aligned(16))) char stack0[128 * NCPU];

static int volatile counter = 0;

static uint64_t volatile bitmask[NCPU];

static bool volatile wait = 0;

extern void main(int threadid) {
  timestamp_t timestamp;
  init_timestamp(&timestamp);

  if (threadid == 0) {
    for (int i=0; i < NCPU; i++) bitmask[i] = 0;
    wait = 1;
  }

  while (!wait) {}

  while (threadid && !bitmask[threadid-1]) {}

  print_stats(threadid, &timestamp);
  bitmask[threadid] = 1;
}
