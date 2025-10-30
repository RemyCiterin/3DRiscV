#include "geometry.h"
#include <stdint.h>
#include "stdlib.h"

static fixed fixed_floor(fixed x) {
  return (x / FIXED_SCALE) * FIXED_SCALE;
}

static fixed fixed_abs(fixed x) {
  return x > 0 ? x : -x;
}

fixed fixed_sin(fixed x) {
  fixed pi = (fixed)(3.141592653589793f * FIXED_SCALE);

  // Rescale x in [-1,1]
  x = fixed_div(x, pi) + FIXED_SCALE;
  x = 2 * (x/2 - fixed_floor(x / 2)) - FIXED_SCALE;

  return 4 * fixed_mul(x, FIXED_SCALE - fixed_abs(x));
}

fixed fixed_cos(fixed x) {
  fixed pi = (fixed)(3.141592653589793f * FIXED_SCALE);
  return fixed_sin(pi / 2 - x);
}

fixed fixed_tan(fixed x) {
  // see https://andrewkay.name/blog/post/efficiently-approximating-tan-x/
  fixed pisqby4 = (fixed)(2.4674011002723397f * FIXED_SCALE);
  fixed adjpisqby4 = (fixed)(2.471688400562703f * FIXED_SCALE);
  fixed adj1minus8bypisq = (fixed)(0.189759681063053f * FIXED_SCALE);
  fixed xsq = fixed_mul(x, x);
  return fixed_div(
    fixed_mul(x, (adjpisqby4 - fixed_mul(adj1minus8bypisq, xsq))),
    (pisqby4 - xsq)
  );
}

void print_fixed(fixed x) {
  fixed a = x > 0 ? x : -x;
  int i = a >> FIXED_LOG_SCALE;
  int f = a & (FIXED_SCALE - 1);

  f = (f * 10000) / FIXED_SCALE;

  if (x < 0) printf("-");

  printf("%d.", i);

  if (f >= 1000) printf("%d", f);
  else if (f >= 100) printf("0%d", f);
  else if (f >= 10) printf("00%d", f);
  else printf("000%d", f);
}

void print_fixed2(fixed2 point) {
  printf("(");
  print_fixed(point.x);
  printf(", ");
  print_fixed(point.y);
  printf(")");
}

void print_fixed3(fixed3 point) {
  printf("(");
  print_fixed(point.x);
  printf(", ");
  print_fixed(point.y);
  printf(", ");
  print_fixed(point.z);
  printf(")");
}

inline fixed3 fixed3_add(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= a.x+b.x, .y= a.y+b.y, .z= a.z+b.z};
  return ret;
}

inline fixed2 fixed2_add(fixed2 a, fixed2 b) {
  fixed2 ret = {.x= a.x+b.x, .y= a.y+b.y};
  return ret;
}

inline fixed3 fixed3_sub(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= a.x-b.x, .y= a.y-b.y, .z= a.z-b.z};
  return ret;
}

inline fixed2 fixed2_sub(fixed2 a, fixed2 b) {
  fixed2 ret = {.x= a.x-b.x, .y= a.y-b.y};
  return ret;
}

inline fixed3 fixed3_mul(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= fixed_mul(a.x,b.x), .y= fixed_mul(a.y,b.y), .z= fixed_mul(a.z,b.z)};
  return ret;
}

inline fixed2 fixed2_mul(fixed2 a, fixed2 b) {
  fixed2 ret = {.x= fixed_mul(a.x,b.x), .y= fixed_mul(a.y,b.y)};
  return ret;
}

inline fixed3 fixed3_div(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= fixed_div(a.x,b.x), .y= fixed_div(a.y,b.y), .z= fixed_div(a.z,b.z)};
  return ret;
}

inline fixed2 fixed2_div(fixed2 a, fixed2 b) {
  fixed2 ret = {.x= fixed_div(a.x,b.x), .y= fixed_div(a.y,b.y)};
  return ret;
}

inline fixed fixed3_dot(fixed3 a, fixed3 b) {
  return fixed_mul(a.x,b.x) + fixed_mul(a.y,b.y) + fixed_mul(a.z,b.z);
}

inline fixed fixed2_dot(fixed2 a, fixed2 b) {
  return fixed_mul(a.x,b.x) + fixed_mul(a.y,b.y);
}

inline fixed3 fixed3_cross(fixed3 a, fixed3 b) {
  fixed3 ret = {
    .x = fixed_mul(a.y, b.z) - fixed_mul(b.y, a.z),
    .y = fixed_mul(a.z, b.x) - fixed_mul(b.z, a.x),
    .z = fixed_mul(a.x, b.z) - fixed_mul(b.x, a.y)
  };
  return ret;
}

// Arguments:
//  - `angle` is the field of view in radian
//  - `ratio` is the aspect ratio
//  - `far` is the distance to the far plan
//  - `near` is the distance to the near plan
void set_projection_matrix(fixed angle, fixed ratio, fixed far, fixed near, fixed** m) {
  fixed h = fixed_div(
    fixed_from_int(1),
    fixed_tan(
      fixed_div(angle, fixed_from_int(2))
    )
  );

  fixed w = fixed_div(h, ratio);

  for (int i=0; i < 4; i++) {
    for (int j=0; j < 4; j++) {
      m[i][j] = 0;
    }
  }

  m[0][0] = w;
  m[1][1] = h;
  m[2][2] = -fixed_div(far, far - near);
  m[3][2] = -fixed_div(fixed_mul(far, near), far - near);
  m[2][3] = -fixed_from_int(1);
}

fixed3 project_point(fixed** m, fixed3 p) {
    fixed3 ret = {
      .x = fixed_mul(m[0][0], p.x) + fixed_mul(m[1][0], p.y) + fixed_mul(m[2][0], p.z) + m[3][0],
      .y = fixed_mul(m[0][1], p.x) + fixed_mul(m[1][1], p.y) + fixed_mul(m[2][1], p.z) + m[3][1],
      .z = fixed_mul(m[0][2], p.x) + fixed_mul(m[1][2], p.y) + fixed_mul(m[2][2], p.z) + m[3][2],
    };

    fixed w =
      fixed_mul(m[0][3], p.x) + fixed_mul(m[1][3], p.y) + fixed_mul(m[2][3], p.z) + m[3][3];

    if (w != 1) {
      ret.x = fixed_div(ret.x, w);
      ret.y = fixed_div(ret.y, w);
      ret.z = fixed_div(ret.z, w);
    }

    return ret;
}

static inline fixed2 min2(fixed2 a, fixed2 b) {
  fixed2 ret;
  ret.x = a.x > b.x ? b.x : a.x;
  ret.y = a.y > b.y ? b.y : a.y;
  return ret;
}

static inline fixed2 max2(fixed2 a, fixed2 b) {
  fixed2 ret;
  ret.x = a.x < b.x ? b.x : a.x;
  ret.y = a.y < b.y ? b.y : a.y;
  return ret;
}

projtri_t project_triangle(fixed** m, triangle_t tri) {
  projtri_t ret;

  fixed3 v0 = project_point(m, tri.vertex[0]);
  fixed3 v1 = project_point(m, tri.vertex[1]);
  fixed3 v2 = project_point(m, tri.vertex[2]);

  ret.vertex[0].x = v0.x; ret.vertex[0].y = v0.y; ret.z.x = v0.z;
  ret.vertex[1].x = v1.x; ret.vertex[1].y = v1.y; ret.z.y = v1.z;
  ret.vertex[2].x = v2.x; ret.vertex[2].y = v2.y; ret.z.z = v2.z;

  fixed a = ret.vertex[1].x - ret.vertex[0].x;
  fixed b = ret.vertex[2].x - ret.vertex[0].x;

  fixed c = ret.vertex[1].y - ret.vertex[0].y;
  fixed d = ret.vertex[2].y - ret.vertex[0].y;

  ret.inv_det =
    fixed_div(
        fixed_from_int(1),
        fixed_mul(a,d) - fixed_mul(b,c)
    );

  ret.texture = tri.texture;
  ret.u[0] = tri.u[0];
  ret.v[0] = tri.v[0];
  ret.u[1] = tri.u[1];
  ret.v[1] = tri.v[1];
  ret.u[2] = tri.u[2];
  ret.v[2] = tri.v[2];

  ret.bounds.aa = min2(min2(ret.vertex[0], ret.vertex[1]), ret.vertex[2]);
  ret.bounds.bb = max2(max2(ret.vertex[0], ret.vertex[1]), ret.vertex[2]);

  return ret;
}


inline fixed3 intersect_triangle(projtri_t tri, fixed2 point) {
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

