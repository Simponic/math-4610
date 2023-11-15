#include "lizfcm.test.h"

UTEST(eigen, dominant_eigenvalue) {
  Matrix_double *m = InitMatrixWithSize(double, 2, 2, 0.0);
  m->data[0]->data[0] = 2.0;
  m->data[0]->data[1] = -12.0;
  m->data[1]->data[0] = 1.0;
  m->data[1]->data[1] = -5.0;

  Array_double *v_guess = InitArrayWithSize(double, 2, 1.0);
  double tolerance = 0.0001;
  uint64_t max_iterations = 64;

  double expect_dominant_eigenvalue = -2.0;

  double approx_dominant_eigenvalue =
      dominant_eigenvalue(m, v_guess, tolerance, max_iterations);

  EXPECT_NEAR(expect_dominant_eigenvalue, approx_dominant_eigenvalue,
              tolerance);
  free_matrix(m);
  free_vector(v_guess);
}
