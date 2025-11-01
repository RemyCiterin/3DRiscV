#include <stdint.h>
#include <stdbool.h>
#include <alloca.h>
#include "stdlib.h"
#include "stats.h"
#include "geometry.h"

#define NCPU (4 * 16)
#define HWIDTH 320
#define VWIDTH 240
#define PRIMS_BOUNDS 32

static int volatile counter = 0;

int volatile kernel_lock = 0;

volatile uint32_t volatile* rgb_buffer;
volatile uint32_t volatile* z_buffer;

static triangle_t tri;

static projtri_t** ptri;

extern triangle_t triangles[PRIMS_BOUNDS];

static fixed proj_body[4][4];
static fixed* proj[4];

timestamp_t volatile start_timestamp;
timestamp_t volatile finish_timestamp;
uint32_t volatile bitmask[NCPU];

// The frame buffer is just an array of 8 bits integers
static uint8_t volatile* FRAME_BASE = (uint8_t volatile*)0x70000000;

static char raw_texture_data[] = "          _____                   /\\    \\                 /::\\    \\               /::::\\    \\             /::::::\\    \\           /:::/\\:::\\    \\         /:::/__\\:::\\    \\       /::::\\   \\:::\\    \\     /::::::\\   \\:::\\    \\   /:::/\\:::\\   \\:::\\    \\ /:::/  \\:::\\   \\:::\\____\\\\::/    \\:::\\  /:::/    / \\/____/ \\:::\\/:::/    /           \\::::::/    /             \\::::/    /              /:::/    /              /:::/    /              /:::/    /              /:::/    /               \\::/    /                 \\/____/         ";

static int texture_data[25*30];

static texture_t texture = {
  .width = 25,
  .height = 30,
  .data = NULL
};

static void free_cube(projtri_t** cube) {
  for (int i=0; i < 12; i++) {
    free(cube[i]);
  }

  free(cube);
}

// Return a cube using a given texture file and the cube coordinates
static projtri_t** mk_cube(texture_t* texture, fixed3 center, fixed size, fixed angle) {
  projtri_t **ret = (projtri_t**)malloc(12 * sizeof(projtri_t*));
  if (!ret) {printf("panic: out of memory\n");}

  triangle_t *triangles = (triangle_t*)malloc(12 * sizeof(triangle_t));
  if (!triangles) {printf("panic: out of memory\n");}

  fixed unit = size / 2;

  ////////////////////////////////////////////////////////////////////////////
  // Initialization of the centered triangles
  ////////////////////////////////////////////////////////////////////////////
  int tri_count = 0;
  for (int axis=0; axis < 3; axis++) {
    for (int dir=0; dir < 2; dir++) {
      fixed plane = dir == 0 ? unit : -unit;

      fixed3 a =
        axis == 0 ? mk_fixed3(plane, unit, unit) :
        axis == 1 ? mk_fixed3(unit, plane, unit) :
        mk_fixed3(unit, unit, plane);

      fixed3 b =
        axis == 0 ? mk_fixed3(plane, -unit, unit) :
        axis == 1 ? mk_fixed3(-unit, plane, unit) :
        mk_fixed3(-unit, unit, plane);

      fixed3 c =
        axis == 0 ? mk_fixed3(plane, -unit, -unit) :
        axis == 1 ? mk_fixed3(-unit, plane, -unit) :
        mk_fixed3(-unit, -unit, plane);

      fixed3 d =
        axis == 0 ? mk_fixed3(plane, unit, -unit) :
        axis == 1 ? mk_fixed3(unit, plane, -unit) :
        mk_fixed3(unit, -unit, plane);

      int* data = (int*)malloc(sizeof(int) * 9);
      for (int i=0; i < 9; i++) data[i] = 5 + axis * 2 + dir;

      texture_t* tex = (texture_t*)malloc(sizeof(texture_t));
      tex->width = 3;
      tex->height = 3;
      tex->data = data;

      triangles[tri_count].texture = tex;
      triangles[tri_count].vertex[0] = a;
      triangles[tri_count].vertex[1] = b;
      triangles[tri_count].vertex[2] = c;
      triangles[tri_count].u[0] = 1;
      triangles[tri_count].u[1] = 1;
      triangles[tri_count].u[2] = 1;
      triangles[tri_count].v[0] = 1;
      triangles[tri_count].v[1] = 1;
      triangles[tri_count].v[2] = 1;
      tri_count++;

      triangles[tri_count].texture = tex;
      triangles[tri_count].vertex[0] = a;
      triangles[tri_count].vertex[1] = c;
      triangles[tri_count].vertex[2] = d;
      triangles[tri_count].u[0] = 1;
      triangles[tri_count].u[1] = 1;
      triangles[tri_count].u[2] = 1;
      triangles[tri_count].v[0] = 1;
      triangles[tri_count].v[1] = 1;
      triangles[tri_count].v[2] = 1;
      tri_count++;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Translate and rotate the triangles
  ////////////////////////////////////////////////////////////////////////////
  fixed one = FIXED_SCALE;
  fixed sin = fixed_sin(angle);
  fixed cos = fixed_cos(angle);
  for (int i=0; i < 12; i++) {

    for (int j=0; j < 3; j++) {
      fixed3 point = triangles[i].vertex[j];
      triangles[i].vertex[j].x = fixed3_dot(mk_fixed3(cos, 0, sin), point);
      triangles[i].vertex[j].y = fixed3_dot(mk_fixed3(0, one, 0), point);
      triangles[i].vertex[j].z = fixed3_dot(mk_fixed3(-sin, 0, cos), point);

      triangles[i].vertex[j] = fixed3_add(center, triangles[i].vertex[j]);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Initialize projection matrix
  ////////////////////////////////////////////////////////////////////////////
  proj[0] = &proj_body[0][0];
  proj[1] = &proj_body[1][0];
  proj[2] = &proj_body[2][0];
  proj[3] = &proj_body[3][0];

  set_projection_matrix(
      (fixed)(FIXED_SCALE * (3.14159 / 4)), // field of view
      (fixed)(FIXED_SCALE * (320.f/240.f)), // aspect ratio
      fixed_from_int(2),                    // far plan distance
      fixed_from_int(1),                    // near plan distance
      proj                                  // projection matrix
  );

  for (int i=0; i < 12; i++) {
    projtri_t* p = (projtri_t*)malloc(sizeof(projtri_t));
    *p = project_triangle(proj, triangles[i]);
    ret[i] = p;
  }

  //for (int i=0; i < 12; i++) {
  //  printf("\ntriangle %d:\n", i);
  //  printf("  "); print_fixed2(ret[i]->vertex[0]); printf("\n");
  //  printf("  "); print_fixed2(ret[i]->vertex[1]); printf("\n");
  //  printf("  "); print_fixed2(ret[i]->vertex[2]); printf("\n");
  //  printf("  "); print_fixed3(ret[i]->z); printf("\n");
  //}

  free(triangles);

  return ret;
}

extern void cpu_main() {
  ////////////////////////////////////////////////////////////////////////////
  // Initialize RGB and Z buffers
  ////////////////////////////////////////////////////////////////////////////
  rgb_buffer = malloc(HWIDTH * VWIDTH * sizeof(int));
  if (!rgb_buffer) {
    printf("PANIC: out of memory\n");
    return;
  }

  z_buffer = malloc(HWIDTH * VWIDTH * sizeof(int));
  if (!z_buffer) {
    printf("PANIC: out of memory\n");
    return;
  }

  for (int i=0; i < VWIDTH; i++) {
    for (int j=0; j < HWIDTH; j++) {
      z_buffer[i*HWIDTH+j] = (1 << 30) - 1;
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  // Initialize textures
  ////////////////////////////////////////////////////////////////////////////
  for (int i=0; raw_texture_data[i]; i++) {
    texture_data[i] = (int)raw_texture_data[i];
  }

  texture.data = texture_data;
  tri.texture = &texture;

  while (true) {
    ////////////////////////////////////////////////////////////////////////////
    // Initialize primitive array
    ////////////////////////////////////////////////////////////////////////////
    init_timestamp((timestamp_t*)&start_timestamp);
    fixed pi = (fixed)(3.141592653589793f * FIXED_SCALE);
    ptri = mk_cube(&texture, mk_fixed3(0,0,FIXED_SCALE*5), FIXED_SCALE, (pi * kernel_lock) / 10);
    init_timestamp((timestamp_t*)&finish_timestamp);

    print_stats(1, (timestamp_t*)&start_timestamp, (timestamp_t*)&finish_timestamp);

    ////////////////////////////////////////////////////////////////////////////
    // Synchronize GPU threads
    ////////////////////////////////////////////////////////////////////////////
    set_print_device(DEVICE_GPU);
    for (int i=0; i < NCPU; i++) bitmask[i] = 0;
    kernel_lock++;

    ////////////////////////////////////////////////////////////////////////////
    // Wait for the kernel computation to finish
    ////////////////////////////////////////////////////////////////////////////
    for (int i=0; i < NCPU; i++) while (!bitmask[i]) {}
    set_print_device(DEVICE_CPU);

    ////////////////////////////////////////////////////////////////////////////
    // Blink LED
    ////////////////////////////////////////////////////////////////////////////
    volatile uint8_t* LED = (volatile uint8_t*)0x10000004;
    *LED = 0x55;

    ////////////////////////////////////////////////////////////////////////////
    // Show GPU statistics
    ////////////////////////////////////////////////////////////////////////////
    print_stats(1, (timestamp_t*)&start_timestamp, (timestamp_t*)&finish_timestamp);

    ////////////////////////////////////////////////////////////////////////////
    // Display screen buffer content
    ////////////////////////////////////////////////////////////////////////////
    init_timestamp((timestamp_t*)&start_timestamp);
    for (int i=0; i < VWIDTH; i++) {
      for (int j=0; j < HWIDTH; j++) {
        FRAME_BASE[i*HWIDTH+j] = (uint8_t)rgb_buffer[i*HWIDTH+j];
      }

    }
    init_timestamp((timestamp_t*)&finish_timestamp);

    ////////////////////////////////////////////////////////////////////////////
    // Show CPU statistics
    ////////////////////////////////////////////////////////////////////////////
    print_stats(1, (timestamp_t*)&start_timestamp, (timestamp_t*)&finish_timestamp);

    ////////////////////////////////////////////////////////////////////////////
    // Free all the data structure used for the cube generation
    ////////////////////////////////////////////////////////////////////////////
    free_cube(ptri);
  }

  printf("End of demo!\n");
  while (true) {}
}

extern int kernel(int, int*, int*, projtri_t**, int);

extern int set_texture(int*);

extern void gpu_main(int threadid) {
  int expected = 1;

  while (true) {
    while (kernel_lock != expected) {}
    kernel(threadid, (int*)rgb_buffer, (int*)z_buffer, ptri, 12);
    expected++;
  }
}
