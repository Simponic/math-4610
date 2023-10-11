#include "lizfcm.h"
#include <stdio.h>
#include <stdlib.h>

double f(double x) { return (x - 1) / (x + 1); }

int main() {
  printf("smaceps(): %.10e\n", smaceps());
  printf("dmaceps(): %.10e\n", dmaceps());

  Array_double *v = InitArray(double, {3, 1, -4, 1, 5, -9, 3});
  Array_double *w = InitArray(double, {-2, 7, 1, -8, -2, 8, 5});

  char v_s[256];
  char w_s[256];
  format_vector_into(v, v_s);
  format_vector_into(w, w_s);

  printf("v: %s\n", v_s);
  printf("w: %s\n", w_s);
  printf("l1_norm(v): %f\n", l1_norm(v));
  printf("l2_norm(v): %f\n", l2_norm(v));
  printf("linf_norm(v): %f\n", linf_norm(v));

  printf("l1_dist(v, w): %f\n", l1_distance(v, w));
  printf("l2_dist(v, w): %f\n", l2_distance(v, w));
  printf("linf_dist(v, w): %f\n", linf_distance(v, w));

  double h = 0.001;
  printf("approx f'(1) w/ c.d.: %f\n", central_derivative_at(&f, 1, h));
  printf("approx f'(1) w/ fw.d.: %f\n", forward_derivative_at(&f, 1, h));
  printf("approx f'(1) w/ bw.d.: %f\n", backward_derivative_at(&f, 1, h));

  v = InitArray(double, {1, 2, 3, 4, 5});
  w = InitArray(double, {2, 3, 4, 5, 6});
  format_vector_into(v, v_s);
  format_vector_into(w, w_s);
  printf("v: %s\n", v_s);
  printf("w: %s\n", w_s);

  Line *line = least_squares_lin_reg(v, w);
  printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);

  v = InitArray(double, {1, 2, 3, 4, 5, 6, 7});
  w = InitArray(double, {0.5, 3, 2, 3.5, 5, 6, 7.5});
  format_vector_into(v, v_s);
  format_vector_into(w, w_s);
  printf("v: %s\n", v_s);
  printf("w: %s\n", w_s);
  line = least_squares_lin_reg(v, w);
  printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);

  return 0;
}
