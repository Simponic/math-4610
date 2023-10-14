#include "lizfcm.h"
#include <assert.h>

Line *least_squares_lin_reg(Array_double *x, Array_double *y) {
  assert(x->size == y->size);

  uint64_t n = x->size;
  double sum_x = sum_v(x);
  double sum_y = sum_v(y);
  double sum_xy = v_dot_v(x, y);
  double sum_xx = v_dot_v(x, x);
  double denom = ((n * sum_xx) - (sum_x * sum_x));

  Line *line = malloc(sizeof(Line));
  line->m = ((sum_xy * n) - (sum_x * sum_y)) / denom;
  line->a = ((sum_y * sum_xx) - (sum_x * sum_xy)) / denom;

  return line;
}
