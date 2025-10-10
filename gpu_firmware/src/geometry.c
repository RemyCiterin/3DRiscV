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
  printf("\n");

  simt_pop();
}

inline fixed3 fixed3_add(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= a.x+b.x, .y= a.y+b.y, .z= a.z+b.z};
  return ret;
}

inline fixed3 fixed3_sub(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= a.x-b.x, .y= a.y-b.y, .z= a.z-b.z};
  return ret;
}

inline fixed3 fixed3_mul(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= fixed_mul(a.x,b.x), .y= fixed_mul(a.y,b.y), .z= fixed_mul(a.z,b.z)};
  return ret;
}

inline fixed3 fixed3_div(fixed3 a, fixed3 b) {
  fixed3 ret = {.x= fixed_div(a.x,b.x), .y= fixed_div(a.y,b.y), .z= fixed_div(a.z,b.z)};
  return ret;
}

inline fixed fixed3_dot(fixed3 a, fixed3 b) {
  return fixed_mul(a.x,b.x) + fixed_mul(a.y,b.y) + fixed_mul(a.z,b.z);
}

inline fixed3 fixed3_cross(fixed3 a, fixed3 b) {
  fixed3 ret = {
    .x = fixed_mul(a.y, b.z) - fixed_mul(b.y, a.z),
    .y = fixed_mul(a.z, b.x) - fixed_mul(b.z, a.x),
    .z = fixed_mul(a.x, b.z) - fixed_mul(b.x, a.y)
  };
  return ret;
}

void set_projection_matrix(fixed angle, fixed far, fixed near, fixed** m) {
  fixed pi_div_2 = (fixed)((3.14159265f / 2.0f) * FIXED_SCALE);
  fixed scale = fixed_div(
    fixed_from_int(1),
    fixed_tan(
      fixed_div(fixed_mul(angle, pi_div_2), fixed_from_int(180))
    )
  );

  simt_push();
  for (int i=0; i < 4; i++) {
    for (int j=0; j < 4; j++) {
      m[i][j] = 0;
    }
  }
  simt_pop();

  m[0][0] = scale;
  m[1][1] = scale;
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
