#include <stdint.h>
#include <stdbool.h>
#include <alloca.h>
#include "stdlib.h"
#include "stats.h"
#include "geometry.h"

#define NCPU (4 * 16)
#define HWIDTH 60 // 320
#define VWIDTH 40 // 240
#define PRIMS_BOUNDS 0x100

static int volatile counter = 0;

static uint64_t volatile bitmask[NCPU];

static bool volatile wait = 0;

static fixed z_buffer[HWIDTH * VWIDTH];
static uint8_t rgb_buffer[HWIDTH * VWIDTH];


static triangle_t tri;
static projtri_t ptri;

extern triangle_t triangles[PRIMS_BOUNDS];

static fixed proj_body[4][4];
static fixed* proj[4];

static bool volatile hit[HWIDTH * VWIDTH];

//static int m1[NCPU][NCPU];
//static int m2[NCPU][NCPU];
//static int m3[NCPU][NCPU];

//static int* FRAME_BASE = 0x70000000;

static char texture[] = "          _____                   /\\    \\                 /::\\    \\               /::::\\    \\             /::::::\\    \\           /:::/\\:::\\    \\         /:::/__\\:::\\    \\       /::::\\   \\:::\\    \\     /::::::\\   \\:::\\    \\   /:::/\\:::\\   \\:::\\    \\ /:::/  \\:::\\   \\:::\\____\\\\::/    \\:::\\  /:::/    / \\/____/ \\:::\\/:::/    /           \\::::::/    /             \\::::/    /              /:::/    /              /:::/    /              /:::/    /              /:::/    /               \\::/    /                 \\/____/         ";

static inline char read_texture(int u, int v) {
  return texture[u*25+v];
}


int fibo(int n) {
  int ret = n;

  simt_push();
  if (n > 1)
    ret = fibo(n-1) + fibo(n-2);
  simt_pop();

  return ret;
}

inline fixed fixed_mul2(fixed a, fixed b) {
  int64_t res = (int64_t)a * (int64_t)b;
  return (fixed)(res >> FIXED_LOG_SCALE);
}
inline fixed3 intersect_triangle2(projtri_t tri, fixed2 point) {
  fixed3 ret;

  fixed a = tri.vertex[1].x - tri.vertex[0].x;
  fixed b = tri.vertex[2].x - tri.vertex[0].x;

  fixed c = tri.vertex[1].y - tri.vertex[0].y;
  fixed d = tri.vertex[2].y - tri.vertex[0].y;

  fixed x = point.x - tri.vertex[0].x;
  fixed y = point.y - tri.vertex[0].y;

  ret.y = fixed_mul2(tri.inv_det, fixed_mul2(d,x) - fixed_mul2(b,y));

  ret.z = fixed_mul2(tri.inv_det, fixed_mul2(y,a) - fixed_mul2(c,x));

  ret.x = fixed_from_int(1) - ret.z - ret.y;

  return ret;
}

inline fixed fixed_div2(fixed n, fixed d) {
  simt_push();
  int64_t a = (int64_t)(n) << FIXED_LOG_SCALE;
  int64_t b = (int64_t)(d);
  fixed ret = (fixed)(a / b);
  simt_pop();
  return ret;
}


void draw_image(int threadid) {
  simt_push();

  for (int idx=threadid; idx < HWIDTH*VWIDTH; idx+=NCPU) {
    rgb_buffer[idx] = ' ';
    z_buffer[idx] = 1 << 30;
  }

  simt_sync();

  fixed xstep = fixed_div2(2*FIXED_SCALE, FIXED_SCALE * HWIDTH);
  fixed ystep = fixed_div2(2*FIXED_SCALE, FIXED_SCALE * VWIDTH);

  simt_sync();

  int xpos = threadid;
  int ypos = 0;

  for (int idx=threadid; idx < HWIDTH*VWIDTH; idx+=NCPU) {
    fixed2 point = {
      .x = fixed_mul2(FIXED_SCALE*xpos, xstep) - FIXED_SCALE,
      .y = fixed_mul2(FIXED_SCALE*ypos, ystep) - FIXED_SCALE
    };

    simt_push();
    bool in_bounds =
      ptri.bounds.aa.x <= point.x && point.x <= ptri.bounds.bb.x &&
      ptri.bounds.aa.y <= point.y && point.y <= ptri.bounds.bb.y;

    if (in_bounds) {
      fixed3 inter = intersect_triangle2(ptri, point);

      bool res = inter.x >= 0 && inter.y >= 0 && inter.z >= 0;

      fixed z = fixed3_dot(inter, ptri.z);

      simt_push();
      if (res && z_buffer[idx] > z && z >= FIXED_SCALE) {
        fixed u =
          inter.x * (fixed)(ptri.u[0]) +
          inter.y * (fixed)(ptri.u[1]) +
          inter.z * (fixed)(ptri.u[2]);
        fixed v =
          inter.x * (fixed)(ptri.v[0]) +
          inter.y * (fixed)(ptri.v[1]) +
          inter.z * (fixed)(ptri.v[2]);

        rgb_buffer[idx] = read_texture(u >> FIXED_LOG_SCALE, v >> FIXED_LOG_SCALE);
        z_buffer[idx] = z;
        hit[idx] = true;
      }
      simt_pop();
    }

    simt_pop();


    // Update coordinates
    simt_push();
    xpos += NCPU;
    while (xpos >= HWIDTH) {
      xpos -= HWIDTH;
      ypos += 1;
    }

    if (ypos >= VWIDTH) {
      ypos = 0;
    }
    simt_pop();
  }

  simt_pop();
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

    ////////////////////////////////////////////////////////////////////////////
    // Initialize triangles
    ////////////////////////////////////////////////////////////////////////////
    tri.vertex[0].x = (fixed)(FIXED_SCALE * -0.8f);
    tri.vertex[0].y = (fixed)(FIXED_SCALE * -0.7f);
    tri.vertex[0].z = (fixed)(FIXED_SCALE * -3.0f);

    tri.vertex[1].x = (fixed)(FIXED_SCALE * -0.8f);
    tri.vertex[1].y = (fixed)(FIXED_SCALE * 0.75f);
    tri.vertex[1].z = (fixed)(FIXED_SCALE * -3.0f);

    tri.vertex[2].x = (fixed)(FIXED_SCALE * -0.08f);
    tri.vertex[2].y = (fixed)(FIXED_SCALE * -0.7f);
    tri.vertex[2].z = (fixed)(FIXED_SCALE * -3.0f);

    tri.u[0] = 0;
    tri.v[0] = 0;

    tri.u[1] = 23;
    tri.v[1] = 0;

    tri.u[2] = 0;
    tri.v[2] = 23;

    ////////////////////////////////////////////////////////////////////////////
    // Initialize projection matrix
    ////////////////////////////////////////////////////////////////////////////
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


    ////////////////////////////////////////////////////////////////////////////
    // Apply the projection matrix to the current triangles
    ////////////////////////////////////////////////////////////////////////////
    ptri = project_triangle(proj, tri);
    printf("vertex0: x: ");
    print_fixed(ptri.vertex[0].x); printf(" y: ");
    print_fixed(ptri.vertex[0].y); printf(" z: ");
    print_fixed(ptri.z.x); printf("\n");

    printf("vertex1: x: ");
    print_fixed(ptri.vertex[1].x); printf(" y: ");
    print_fixed(ptri.vertex[1].y); printf(" z: ");
    print_fixed(ptri.z.y); printf("\n");

    printf("vertex2: x: ");
    print_fixed(ptri.vertex[2].x); printf(" y: ");
    print_fixed(ptri.vertex[2].y); printf(" z: ");
    print_fixed(ptri.z.z); printf("\n");

    print_fixed2(ptri.bounds.aa); printf("\n");
    print_fixed2(ptri.bounds.bb); printf("\n");


    ////////////////////////////////////////////////////////////////////////////
    // Synchronize other threads
    ////////////////////////////////////////////////////////////////////////////
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
  if (threadid == 0) print_stats(threadid, &timestamp);
  //print_stats(threadid, &timestamp);

  //for (int i=0; i < NCPU; i++) {
  //  printf("%d ", m3[i][threadid]);
  //}
  //printf("\n");

  // Allow the next thread to start
  bitmask[threadid] = 1;

  if (threadid == NCPU-1) {
    for (int i=0; i < VWIDTH; i++) {
      char* line = alloca(HWIDTH+1);

      for (int j=0; j < HWIDTH; j++)
        line[j] = rgb_buffer[i*HWIDTH+j];
      line[HWIDTH] = 0;

      printf("%s\n", line);
    }
  }
}
