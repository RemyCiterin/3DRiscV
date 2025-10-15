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

timestamp_t global_timestamp;

//static int m1[NCPU][NCPU];
//static int m2[NCPU][NCPU];
//static int m3[NCPU][NCPU];

//static int* FRAME_BASE = 0x70000000;

static char texture[] = "          _____                   /\\    \\                 /::\\    \\               /::::\\    \\             /::::::\\    \\           /:::/\\:::\\    \\         /:::/__\\:::\\    \\       /::::\\   \\:::\\    \\     /::::::\\   \\:::\\    \\   /:::/\\:::\\   \\:::\\    \\ /:::/  \\:::\\   \\:::\\____\\\\::/    \\:::\\  /:::/    / \\/____/ \\:::\\/:::/    /           \\::::::/    /             \\::::/    /              /:::/    /              /:::/    /              /:::/    /              /:::/    /               \\::/    /                 \\/____/         ";

static inline char read_texture(int u, int v) {
  return texture[u*25+v];
}

inline fixed3 intersect_triangle2(projtri_t tri, fixed2 point) {
  fixed3 ret;

  fixed a = tri.vertex[1].x - tri.vertex[0].x;
  fixed b = tri.vertex[2].x - tri.vertex[0].x;

  fixed c = tri.vertex[1].y - tri.vertex[0].y;
  fixed d = tri.vertex[2].y - tri.vertex[0].y;

  fixed x = point.x - tri.vertex[0].x;
  fixed y = point.y - tri.vertex[0].y;

  ret.y = fixed_mul(tri.inv_det, fixed_mul(d,x) - fixed_mul(b,y));

  ret.z = fixed_mul(tri.inv_det, fixed_mul(y,a) - fixed_mul(c,x));

  ret.x = fixed_from_int(1) - ret.z - ret.y;

  return ret;
}


void draw_image(int threadid) {
  simt_push();

  for (int idx=threadid; idx < HWIDTH*VWIDTH; idx+=NCPU) {
    rgb_buffer[idx] = ' ';
    z_buffer[idx] = 1 << 30;
  }

  simt_sync();

  fixed xstep = fixed_div(2*FIXED_SCALE, FIXED_SCALE * HWIDTH);
  fixed ystep = fixed_div(2*FIXED_SCALE, FIXED_SCALE * VWIDTH);

  simt_sync();

  int xpos = threadid;
  int ypos = 0;

  for (int idx=threadid; idx < HWIDTH*VWIDTH; idx+=NCPU) {
    fixed2 point = {
      .x = fixed_mul(FIXED_SCALE*xpos, xstep) - FIXED_SCALE,
      .y = fixed_mul(FIXED_SCALE*ypos, ystep) - FIXED_SCALE
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

    simt_sync();

    // Update coordinates
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

extern void cpu_main() {
  init_timestamp(&global_timestamp);

  while (!bitmask[NCPU-1]) {}

  print_stats(0, &global_timestamp);

  for (int i=0; i < VWIDTH; i++) {
    char* line = alloca(HWIDTH+1);

    for (int j=0; j < HWIDTH; j++)
      line[j] = rgb_buffer[i*HWIDTH+j];
    line[HWIDTH] = 0;

    for (int j=HWIDTH-1; i > 0; j--) {
      if (line[j] != ' ') {
        line[j+1] = 0;
        break;
      }
    }

    printf("%s\n", line);
  }

  while (true) {
    printf("Hello world!\n");
  }
}

extern void gpu_main(int threadid) {
  // Initialize thread locks
  if (threadid == 0) {

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


    ////////////////////////////////////////////////////////////////////////////
    // Synchronize other threads
    ////////////////////////////////////////////////////////////////////////////
    wait = 1;
  }

  while (!wait) {}

  simt_sync();

  draw_image(threadid);

  // Wait for the execution of all the previous threads to finish
  while (threadid && !bitmask[threadid-1]) {}


  // Allow the next thread to start
  bitmask[threadid] = 1;
}
