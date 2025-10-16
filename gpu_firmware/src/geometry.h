#pragma once

#include <stdbool.h>
#include <stdint.h>
#include "stdlib.h"

#define FIXED_LOG_SCALE 16
#define FIXED_SCALE (1 << FIXED_LOG_SCALE)

typedef int fixed;

typedef struct {
  fixed x;
  fixed y;
  fixed z;
} fixed3;

typedef struct {
  fixed x;
  fixed y;
} fixed2;

typedef struct {
  fixed2 aa;
  fixed2 bb;
} aabb_t;

// Representation of a triangle in 3D
typedef struct {
  // Position of each pixel in the 3D space
  fixed3 vertex[3];

  // Texture ID
  uint8_t texture;

  // U-Coordinates of each vertex in the texture buffer
  uint16_t u[3];

  // V-Coordinates of each vertex in the texture buffer
  uint16_t v[3];
} triangle_t;

// Projection of a triangle in 2D
typedef struct __attribute__((packed)) {
  // Determinant of the matrix
  // | a b |
  // | c d |
  fixed inv_det;

  // Position of each vertex in the screen
  fixed2 vertex[3];

  // Position of each vertex in the Z axis (used for the Z buffer)
  fixed3 z;

  // Bounds of the triangle
  aabb_t bounds;

  // U-Coordinates of each vertex in the texture buffer
  uint16_t u[3];

  // V-Coordinates of each vertex in the texture buffer
  uint16_t v[3];

  // Texture ID
  uint8_t texture;
} projtri_t;

inline fixed fixed_mul(fixed a, fixed b) {
  int64_t res = (int64_t)a * (int64_t)b;
  return (fixed)(res >> FIXED_LOG_SCALE);
}

inline fixed fixed_from_int(int x) {
  return x << FIXED_LOG_SCALE;
}

inline fixed fixed_div(fixed n, fixed d) {
  simt_push();
  int64_t a = (int64_t)(n) << FIXED_LOG_SCALE;
  int64_t b = (int64_t)(d);
  fixed ret = (fixed)(a / b);
  simt_pop();
  return ret;
}

void print_fixed(fixed);

void print_fixed2(fixed2);

void print_fixed3(fixed3);

fixed fixed_tan(fixed);

fixed3 fixed3_add(fixed3, fixed3);

fixed2 fixed2_add(fixed2, fixed2);

fixed3 fixed3_sub(fixed3, fixed3);

fixed2 fixed2_sub(fixed2, fixed2);

fixed3 fixed3_mul(fixed3, fixed3);

fixed2 fixed2_mul(fixed2, fixed2);

fixed3 fixed3_div(fixed3, fixed3);

fixed2 fixed2_div(fixed2, fixed2);

fixed fixed3_dot(fixed3, fixed3);

fixed fixed2_dot(fixed2, fixed2);

fixed3 fixed3_cross(fixed3, fixed3);

// Arguments:
//  - `angle` is the field of view in radian
//  - `ratio` is the aspect ratio
//  - `far` is the distance to the far plan
//  - `near` is the distance to the near plan
void set_projection_matrix(fixed angle, fixed ratio, fixed far, fixed near, fixed** m);

fixed3 project_point(fixed**, fixed3);

projtri_t project_triangle(fixed**, triangle_t);

fixed3 intersect_triangle(projtri_t, fixed2);

