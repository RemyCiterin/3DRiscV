
#define FIXED_LOG_SCALE 16
#define FIXED_SCALE (1 << FIXED_LOG_SCALE)

typedef int fixed;

fixed fixed_from_int(int);

void print_fixed(fixed);

fixed fixed_mul(fixed, fixed);

fixed fixed_div(fixed, fixed);

fixed fixed_tan(fixed);

typedef struct {
  fixed x;
  fixed y;
  fixed z;
} fixed3;

fixed3 fixed3_add(fixed3, fixed3);

fixed3 fixed3_sub(fixed3, fixed3);

fixed3 fixed3_mul(fixed3, fixed3);

fixed3 fixed3_div(fixed3, fixed3);

fixed fixed3_dot(fixed3, fixed3);

fixed3 fixed3_cross(fixed3, fixed3);

void set_projection_matrix(fixed angle, fixed ratio, fixed far, fixed near, fixed** m);

fixed3 project_point(fixed** m, fixed3 point);
