#include <stdint.h>
#include <stdbool.h>
#include "stdlib.h"
#include "stats.h"
#include "geometry.h"

#define NCPU (4 * 16)
#define HWIDTH 320
#define VWIDTH 240
#define PRIMS_BOUNDS 0x100

extern __attribute__((aligned(16))) char stack0[128 * NCPU];

static int volatile counter = 0;

static uint64_t volatile bitmask[NCPU];

static bool volatile wait = 0;

extern __attribute__((aligned(64))) fixed z_buffer[HWIDTH * VWIDTH];
extern __attribute__((aligned(64))) uint8_t rgb_buffer[HWIDTH * VWIDTH];


static triangle_t tri;
static projtri_t ptri;

extern __attribute__((aligned(64))) triangle_t triangles[PRIMS_BOUNDS];

static fixed proj_body[4][4];
static fixed* proj[4];

//static int m1[NCPU][NCPU];
//static int m2[NCPU][NCPU];
//static int m3[NCPU][NCPU];

//static int* FRAME_BASE = 0x70000000;

extern void main(int threadid) {
  // Initialize thread locks
  if (threadid == 0) {
    //for (int i=0; i < NCPU; i ++) {
    //  for (int j=0; j < NCPU; j++) {
    //    m1[i][j] = i;
    //  }
    //}
    //for (int i=0; i < NCPU; i ++) {
    //  for (int j=0; j < NCPU; j++) {
    //    m2[i][j] = i;
    //  }
    //}

    for (int i=0; i < NCPU; i++) bitmask[i] = 0;

    tri.vertex[0].x = (fixed)(FIXED_SCALE * 0.f);
    tri.vertex[0].y = (fixed)(FIXED_SCALE * 0.f);
    tri.vertex[0].z = (fixed)(FIXED_SCALE * -3.f);

    tri.vertex[1].x = (fixed)(FIXED_SCALE * 0.f);
    tri.vertex[1].y = (fixed)(FIXED_SCALE * 0.5f);
    tri.vertex[1].z = (fixed)(FIXED_SCALE * -3.f);

    tri.vertex[2].x = (fixed)(FIXED_SCALE * 0.5f);
    tri.vertex[2].y = (fixed)(FIXED_SCALE * 0.f);
    tri.vertex[2].z = (fixed)(FIXED_SCALE * -3.f);

    tri.color = 128;

    ////////////////////////////////////////////////////////////////////////////:
    // Initialize projection matrix
    ////////////////////////////////////////////////////////////////////////////:
    proj[0] = &proj_body[0][0];
    proj[1] = &proj_body[1][0];
    proj[2] = &proj_body[2][0];
    proj[3] = &proj_body[3][0];

    set_projection_matrix(
        (fixed)(FIXED_SCALE * (3.14159 / 4)), // field of view
        (fixed)(FIXED_SCALE * (240.f/320.f)), // aspect ratio
        fixed_from_int(2),                    // far plan distance
        fixed_from_int(1),                    // near plan distance
        proj                                  // projection matrix
    );


    ////////////////////////////////////////////////////////////////////////////:
    // Apply the projection matrix to the current triangles
    ////////////////////////////////////////////////////////////////////////////:
    ptri = project_triangle(proj, tri);
    printf("vertex0: x: ");
    print_fixed(ptri.vertex[0].x); printf(" y: ");
    print_fixed(ptri.vertex[0].y); printf(" z: ");
    print_fixed(ptri.z[0]); printf("\n");

    printf("vertex1: x: ");
    print_fixed(ptri.vertex[1].x); printf(" y: ");
    print_fixed(ptri.vertex[1].y); printf(" z: ");
    print_fixed(ptri.z[1]); printf("\n");

    printf("vertex2: x: ");
    print_fixed(ptri.vertex[2].x); printf(" y: ");
    print_fixed(ptri.vertex[2].y); printf(" z: ");
    print_fixed(ptri.z[2]); printf("\n");


    ////////////////////////////////////////////////////////////////////////////:
    // Synchronize other threads
    ////////////////////////////////////////////////////////////////////////////:
    wait = 1;
  }

  while (!wait) {}

  simt_sync();
  timestamp_t timestamp;
  init_timestamp(&timestamp);
  simt_sync();


  //for (int i=0; i < NCPU; i++) {
  //  for (int j=0; j < NCPU; j++) {
  //    m3[i][threadid] += m1[i][j] * m2[j][threadid];
  //  }
  //}

  int xpos = threadid & 0x7;
  int ypos = threadid >> 3;

  fixed2 point = {
    .x = fixed_div(FIXED_SCALE * xpos, 4*FIXED_SCALE) - FIXED_SCALE,
    .y = fixed_div(FIXED_SCALE * ypos, 4*FIXED_SCALE) - FIXED_SCALE
  };

  fixed3 inter = intersect_triangle(ptri, point);

  bool res = inter.x >= 0 && inter.y >= 0 && inter.z <= FIXED_SCALE;

  // Wait for the execution of all the previous threads to finish
  while (threadid && !bitmask[threadid-1]) {}

  //printf("xpos: %d ypos: %d\n", xpos, ypos);
  print_fixed2(point); printf("\n");
  //print_fixed(inter.x); printf("\n");
  //print_fixed(inter.y); printf("\n");
  //print_fixed(inter.z); printf("\n");
  printf("%d", res);
  if (xpos == 7) printf("\n");

  // Print statistics
  //print_stats(threadid, &timestamp);

  //for (int i=0; i < NCPU; i++) {
  //  printf("%d ", m3[i][threadid]);
  //}
  //printf("\n");

  // Allow the next thread to start
  bitmask[threadid] = 1;
}
