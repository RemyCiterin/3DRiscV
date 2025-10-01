#include <stdint.h>
#include "stdlib.h"

#define NCPU (2 * 16)

__attribute__((aligned(16))) char stack0[128 * NCPU];

extern void main(int threadid) {
  printf("Hello world!\n");
}
