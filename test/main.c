#include "lizfcm.h"
#include <stdio.h>
#include <stdlib.h>

double f(double x) { return (x - 1) / (x + 1); }

int main() {
  printf("Basic Routines\n");
  printf("smaceps(): %.10e\n", smaceps());
  printf("dmaceps(): %.10e\n", dmaceps());
  printf("========\n");

  printf("Norm, Distance\n");
  Array_double *v = InitArray(double, {3, 1, -4, 1, 5, -9, 3});
  char s[2048];
  strcpy(s, "");
  format_vector_into(v, s);
  printf("v: %s", s);

  Array_double *w = InitArray(double, {-2, 7, 1, -8, -2, 8, 5});
  strcpy(s, "");
  format_vector_into(w, s);
  printf("w: %s", s);

  printf("l1_norm(v): %f\n", l1_norm(v));
  printf("l2_norm(v): %f\n", l2_norm(v));
  printf("linf_norm(v): %f\n", linf_norm(v));
  printf("l1_dist(v, w): %f\n", l1_distance(v, w));
  printf("l2_dist(v, w): %f\n", l2_distance(v, w));
  printf("linf_dist(v, w): %f\n", linf_distance(v, w));
  printf("========\n");

  double h = 0.001;
  printf("Derivative Approxs\n");
  printf("f(x) = (x-1)/(x+1)\n");
  printf("approx f'(1) w/ c.d.: %f\n", central_derivative_at(&f, 1, h));
  printf("approx f'(1) w/ fw.d.: %f\n", forward_derivative_at(&f, 1, h));
  printf("approx f'(1) w/ bw.d.: %f\n", backward_derivative_at(&f, 1, h));
  printf("========\n");
  printf("Least Squares\n");

  v = InitArray(double, {1, 2, 3, 4, 5});
  strcpy(s, "");
  format_vector_into(v, s);
  printf("v: %s", s);
  w = InitArray(double, {2, 3, 4, 5, 6});
  strcpy(s, "");
  format_vector_into(w, s);
  printf("w: %s", s);

  Line *line = least_squares_lin_reg(v, w);

  printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);
  v = InitArray(double, {1, 2, 3, 4, 5, 6, 7});
  strcpy(s, "");
  format_vector_into(v, s);
  printf("v: %s", s);
  w = InitArray(double, {0.5, 3, 2, 3.5, 5, 6, 7.5});
  strcpy(s, "");
  format_vector_into(w, s);
  printf("w: %s", s);

  line = least_squares_lin_reg(v, w);
  printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);
  printf("========\n");

  printf("LU Decomp\n");
  Matrix_double *m = InitMatrixWithSize(double, 10, 10, 0.0);
  for (int i = 0; i < 10; i++) {
    for (int j = 0; j < 10; j++)
      m->data[i]->data[j] = (100 - rand() % 200);
  }
  strcpy(s, "");
  format_matrix_into(m, s);
  printf("m = %s", s);

  Array_double *b = InitArrayWithSize(double, 10, 100.0);
  Matrix_double **u_l = lu_decomp(m);
  Matrix_double *u = u_l[0];
  Matrix_double *l = u_l[1];

  strcpy(s, "");
  format_matrix_into(u, s);
  printf("u = %s", s);
  strcpy(s, "");
  format_matrix_into(l, s);
  printf("l = %s", s);
  strcpy(s, "");
  format_vector_into(b, s);
  printf("b = %s", s);
  printf("========\n");
  printf("Backward -> Forward Substitution\n");

  Array_double *b_fsub = fsubst(l, b);
  strcpy(s, "");
  format_vector_into(b_fsub, s);
  printf("x: %s\n", s);

  Array_double *x_bsub = bsubst(u, b_fsub);
  strcpy(s, "");
  format_vector_into(x_bsub, s);
  printf("x: %s", s);

  free_vector(b_fsub);

  printf("Verifications (each should be approximately 100)\n");
  for (size_t row = 0; row < m->rows; row++) {
    double curr = 0;
    for (size_t col = 0; col < m->cols; col++)
      curr += m->data[row]->data[col] * x_bsub->data[col];
    printf("Substitution for row %zu = %f\n", row, curr);
  }

  return 0;
}
