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

bool volatile wait = 0;

volatile uint32_t volatile* rgb_buffer;//[HWIDTH * VWIDTH];

static triangle_t tri;

static projtri_t ptri0;
static projtri_t* ptri[1];

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

extern void cpu_main() {
  ////////////////////////////////////////////////////////////////////////////
  // Initialize RGB buffer
  ////////////////////////////////////////////////////////////////////////////
  rgb_buffer = malloc(HWIDTH * VWIDTH * sizeof(int));
  if (!rgb_buffer) {
    printf("PANIC: out of memory\n");
    return;
  }

  ////////////////////////////////////////////////////////////////////////////
  // Initialize textures
  ////////////////////////////////////////////////////////////////////////////
  for (int i=0; raw_texture_data[i]; i++) {
    texture_data[i] = (int)raw_texture_data[i];
  }

  texture.data = texture_data;
  tri.texture = &texture;

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

  tri.u[1] = 20;
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
  ptri[0] = alloca(sizeof(projtri_t));
  *ptri[0] = project_triangle(proj, tri);

  ////////////////////////////////////////////////////////////////////////////
  // Synchronize other threads
  ////////////////////////////////////////////////////////////////////////////
  wait = 1;

  ////////////////////////////////////////////////////////////////////////////
  // Wait for the kernel computation to finish
  ////////////////////////////////////////////////////////////////////////////
  for (int i=0; i < NCPU; i++) while (!bitmask[i]) {}

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
  //char* line = malloc(HWIDTH+1);
  for (int i=0; i < VWIDTH; i++) {

    //for (int j=0; j < HWIDTH; j++)
    //  line[j] = rgb_buffer[i*HWIDTH+j];
    //line[HWIDTH] = 0;

    //for (int j=HWIDTH-1; j >= 0; j--) {
    //  if (line[j] != ' ') break;
    //  line[j+1] = 0;
    //}

    //printf("%s\n", line);

    for (int j=0; j < HWIDTH; j++) {
      FRAME_BASE[i*HWIDTH+j] = (uint8_t)rgb_buffer[i*HWIDTH+j];
    }

  }
  //free(line);
  init_timestamp((timestamp_t*)&finish_timestamp);

  ////////////////////////////////////////////////////////////////////////////
  // Show CPU statistics
  ////////////////////////////////////////////////////////////////////////////
  print_stats(1, (timestamp_t*)&start_timestamp, (timestamp_t*)&finish_timestamp);



  while (true) {
    printf("Hello world!\n");
  }
}

extern int kernel(int, int*, projtri_t**, int);

extern int set_texture(int*);

extern void gpu_main(int threadid) {
  while (!wait) {}
  kernel(threadid, (int*)rgb_buffer, ptri, 1);
}
