#include "lizfcm.h"
#include <stdio.h>
#include <stdlib.h>

double f(double x) { return (x - 1) / (x + 1); }

int main() {
    printf("Basic Routines\n");
    printf("smaceps(): %.10e\n", smaceps());
    printf("dmaceps(): %.10e\n", dmaceps());
    printf("========\n");
  
    Array_double *v = InitArray(double, {3, 1, -4, 1, 5, -9, 3}); 
    char v_s[256];
    strcpy(v_s, "");
    format_vector_into(v, v_s);

    Array_double *w = InitArray(double, {-2, 7, 1, -8, -2, 8, 5});
    char w_s[256];
    strcpy(w_s, "");
    format_vector_into(w, w_s);
  
    printf("Norm, Distance\n");
    printf("v: %s", v_s);
    printf("w: %s", w_s);
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
  
    v = InitArray(double, {1, 2, 3, 4, 5});
    strcpy(v_s, "");
    format_vector_into(v, v_s);
    w = InitArray(double, {2, 3, 4, 5, 6});
    strcpy(w_s, "");
    format_vector_into(w, w_s);
    Line *line = least_squares_lin_reg(v, w);
    printf("Least Squares\n");
    printf("v: %s", v_s);
    printf("w: %s", w_s); 
    printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);
    v = InitArray(double, {1, 2, 3, 4, 5, 6, 7});
    strcpy(v_s, "");
    format_vector_into(v, v_s);
    w = InitArray(double, {0.5, 3, 2, 3.5, 5, 6, 7.5});
    strcpy(w_s, "");
    format_vector_into(w, w_s);
    printf("v: %s", v_s);
    printf("w: %s", w_s);
    line = least_squares_lin_reg(v, w);
    printf("least_squares_lin_reg(v, w): (%f)x + %f\n", line->m, line->a);
    printf("========\n");

  
  printf("LU Decomp\n");
  char m_s[2048];
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  for (int i = 0; i < 8; i++) {
    for (int j = 0; j < 8; j++) {
      m->data[i]->data[j] = (i + 1.0) + j * 3 + (rand() % 12);
    }
  }
  format_matrix_into(m, m_s);
  printf("m = %s", m_s);

  Matrix_double **u_l = put_lu_decomp(m);
  Matrix_double *u = u_l[0];
  Matrix_double *l = u_l[1];

  strcpy(m_s, "");
  format_matrix_into(u, m_s);
  printf("u = %s", m_s);
  strcpy(m_s, "");
  format_matrix_into(l, m_s);
  printf("l = %s", m_s);
  printf("========\n");
  printf("Back Substitution\n");

  return 0;
}
