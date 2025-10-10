#include "geometry.h"
#include <stdint.h>
#include "stdlib.h"

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

inline fixed fixed_tan(fixed x) {
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
  simt_push();

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

  simt_pop();
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

  simt_push();
  for (int i=0; i < 4; i++) {
    for (int j=0; j < 4; j++) {
      m[i][j] = 0;
    }
  }
  simt_pop();

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

    simt_push();
    if (w != 1) {
      ret.x = fixed_div(ret.x, w);
      ret.y = fixed_div(ret.y, w);
      ret.z = fixed_div(ret.z, w);
    }
    simt_pop();

    return ret;
}

projtri_t project_triangle(fixed** m, triangle_t tri) {
  projtri_t ret;

  fixed3 v0 = project_point(m, tri.vertex[0]);
  fixed3 v1 = project_point(m, tri.vertex[1]);
  fixed3 v2 = project_point(m, tri.vertex[2]);

  print_fixed3(v0); printf("\n");
  print_fixed3(v1); printf("\n");
  print_fixed3(v2); printf("\n");

  ret.vertex[0].x = v0.x; ret.vertex[0].y = v0.y; ret.z[0] = v0.z;
  ret.vertex[1].x = v1.x; ret.vertex[1].y = v1.y; ret.z[1] = v1.z;
  ret.vertex[2].x = v2.x; ret.vertex[2].y = v2.y; ret.z[2] = v2.z;

  fixed a = ret.vertex[1].x - ret.vertex[0].x;
  fixed b = ret.vertex[1].y - ret.vertex[0].y;

  fixed c = ret.vertex[2].x - ret.vertex[0].x;
  fixed d = ret.vertex[2].y - ret.vertex[0].y;

  ret.inv_det =
    fixed_div(
        fixed_from_int(1),
        fixed_mul(a,d) - fixed_mul(b,c)
    );

  print_fixed2(ret.vertex[0]); printf("\n");
  print_fixed2(ret.vertex[1]); printf("\n");
  print_fixed2(ret.vertex[2]); printf("\n");

  ret.color = tri.color;

  return ret;
}


inline fixed3 intersect_triangle(projtri_t tri, fixed2 point) {
  fixed3 ret;

  fixed a = tri.vertex[1].x - tri.vertex[0].x;
  fixed b = tri.vertex[1].y - tri.vertex[0].y;

  fixed c = tri.vertex[2].x - tri.vertex[0].x;
  fixed d = tri.vertex[2].y - tri.vertex[0].y;

  fixed x = point.x - tri.vertex[0].x;
  fixed y = point.y - tri.vertex[0].y;

  ret.x = fixed_mul(tri.inv_det, fixed_mul(d,x) - fixed_mul(b,y));

  ret.x = fixed_mul(tri.inv_det, fixed_mul(y,a) - fixed_mul(c,x));

  ret.z = fixed_from_int(1) - ret.x - ret.y;

  return ret;
}
