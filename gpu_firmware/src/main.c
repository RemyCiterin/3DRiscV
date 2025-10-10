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

static bool volatile hit[32 * 32];

//static int m1[NCPU][NCPU];
//static int m2[NCPU][NCPU];
//static int m3[NCPU][NCPU];

//static int* FRAME_BASE = 0x70000000;
//

inline fixed fixed_mul2(fixed a, fixed b) {
  int64_t res = (int64_t)a * (int64_t)b;
  return (fixed)(res >> FIXED_LOG_SCALE);
}
inline fixed3 intersect_triangle(projtri_t tri, fixed2 point) {
  fixed3 ret;

  fixed a = tri.vertex[1].x - tri.vertex[0].x;
  fixed b = tri.vertex[1].y - tri.vertex[0].y;

  fixed c = tri.vertex[2].x - tri.vertex[0].x;
  fixed d = tri.vertex[2].y - tri.vertex[0].y;

  fixed x = point.x - tri.vertex[0].x;
  fixed y = point.y - tri.vertex[0].y;

  ret.x = fixed_mul2(tri.inv_det, fixed_mul2(d,x) - fixed_mul2(b,y));

  ret.y = fixed_mul2(tri.inv_det, fixed_mul2(y,a) - fixed_mul2(c,x));

  ret.z = fixed_from_int(1) - ret.x - ret.y;

  return ret;
}


void draw_image(int threadid) {
  for (int idx=threadid; idx < 32*32; idx+=NCPU) {
    int xpos = idx & 31;
    int ypos = idx >> 5;


    fixed2 point = {
      .x = 2*((FIXED_SCALE * xpos) >> 5) - FIXED_SCALE,
      .y = 2*((FIXED_SCALE * ypos) >> 5) - FIXED_SCALE
    };

    fixed3 inter = intersect_triangle(ptri, point);

    bool res = inter.x >= 0 && inter.y >= 0 && inter.z >= 0;

    hit[idx] = res;
  }
}


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

    tri.vertex[2].x = (fixed)(FIXED_SCALE * 0.9f);
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

  draw_image(threadid);

  //int xpos = threadid & 0x7;
  //int ypos = threadid >> 3;

  //fixed3 inter = intersect_triangle(ptri, point);

  //bool res = inter.x >= 0 && inter.y >= 0 && inter.z >= 0;

  // Wait for the execution of all the previous threads to finish
  while (threadid && !bitmask[threadid-1]) {}

  //printf("xpos: %d ypos: %d\n", xpos, ypos);
  //print_fixed2(point); printf("\n");
  //print_fixed(inter.x); printf("\n");
  //print_fixed(inter.y); printf("\n");
  //print_fixed(inter.z); printf("\n");
  //printf("%d", res);
  //if (xpos == 7) printf("\n");

  // Print statistics
  print_stats(threadid, &timestamp);

  //for (int i=0; i < NCPU; i++) {
  //  printf("%d ", m3[i][threadid]);
  //}
  //printf("\n");

  // Allow the next thread to start
  bitmask[threadid] = 1;

  if (threadid == NCPU-1) {
    for (int i=0; i < 32; i++) {
      for (int j=0; j < 32; j++) {
        printf("%d", hit[i*32+j]);
      }
      printf("\n");
    }
  }
}
