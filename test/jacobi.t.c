#include "lizfcm.test.h"
#include <math.h>

Matrix_double *generate_ddm(size_t n) {
  Matrix_double *m = InitMatrixWithSize(double, n, n, rand_from(0.0, 1.0));

  for (size_t y = 0; y < m->rows; y++) {
    m->data[y]->data[y] += sum_v(m->data[y]);
  }

  return m;
}

UTEST(jacobi, jacobi_solve) {
  Matrix_double *m = generate_ddm(2);

  Array_double *b_1 = InitArrayWithSize(double, m->rows, 1.0);
  Array_double *b = m_dot_v(m, b_1);

  double tolerance = 0.001;
  size_t max_iter = 400;
  Array_double *solution = jacobi_solve(m, b, tolerance, max_iter);

  for (size_t y = 0; y < m->rows; y++) {
    double dot = v_dot_v(m->data[y], solution);
    EXPECT_NEAR(b->data[y], dot, 0.1);
  }

  free_matrix(m);
  free_vector(b_1);
  free_vector(b);
  free_vector(solution);
}
