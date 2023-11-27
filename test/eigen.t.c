#include "lizfcm.test.h"

Matrix_double *eigen_test_matrix() {
  // produces a matrix that has eigenvalues [5 + sqrt{17}, 2, 5 - sqrt{17}]
  Matrix_double *m = InitMatrixWithSize(double, 3, 3, 0.0);
  m->data[0]->data[0] = 2.0;
  m->data[0]->data[1] = 2.0;
  m->data[0]->data[2] = 4.0;
  m->data[1]->data[0] = 1.0;
  m->data[1]->data[1] = 4.0;
  m->data[1]->data[2] = 7.0;
  m->data[2]->data[1] = 2.0;
  m->data[2]->data[2] = 6.0;
  return m;
}

UTEST(eigen, least_dominant_eigenvalue) {
  Matrix_double *m = eigen_test_matrix();

  double expected_least_dominant_eigenvalue = 0.87689; // 5 - sqrt(17)
  double tolerance = 0.0001;
  uint64_t max_iterations = 64;

  Array_double *v_guess = InitArrayWithSize(double, 3, 1.0);
  double approx_least_dominant_eigenvalue =
      least_dominant_eigenvalue(m, v_guess, tolerance, max_iterations);

  EXPECT_NEAR(expected_least_dominant_eigenvalue,
              approx_least_dominant_eigenvalue, tolerance);
}

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

UTEST(eigen, shifted_eigenvalue) {
  Matrix_double *m = eigen_test_matrix();

  double least_dominant_eigenvalue = 0.87689; // 5 - sqrt{17}
  double dominant_eigenvalue = 9.12311;       // 5 + sqrt{17}
  double expected_middle_eigenvalue = 2.0;
  double shift = (dominant_eigenvalue + least_dominant_eigenvalue) / 2.0;

  double tolerance = 0.0001;
  uint64_t max_iterations = 64;
  Array_double *v_guess = InitArray(double, {0.5, 1.0, 0.75});

  double approx_middle_eigenvalue = shift_inverse_power_eigenvalue(
      m, v_guess, shift, tolerance, max_iterations);

  EXPECT_NEAR(approx_middle_eigenvalue, expected_middle_eigenvalue, tolerance);
}

UTEST(eigen, leslie_matrix_dominant_eigenvalue) {
  Array_double *felicity = InitArray(double, {0.0, 1.5, 0.8});
  Array_double *survivor_ratios = InitArray(double, {0.8, 0.55});
  Matrix_double *leslie = leslie_matrix(survivor_ratios, felicity);
  Array_double *v_guess = InitArrayWithSize(double, 3, 2.0);
  double tolerance = 0.0001;
  uint64_t max_iterations = 64;

  double expect_dominant_eigenvalue = 1.22005;

  double approx_dominant_eigenvalue =
      dominant_eigenvalue(leslie, v_guess, tolerance, max_iterations);

  EXPECT_NEAR(expect_dominant_eigenvalue, approx_dominant_eigenvalue,
              tolerance);

  free_vector(v_guess);
  free_vector(survivor_ratios);
  free_vector(felicity);
  free_matrix(leslie);
}
UTEST(eigen, leslie_matrix) {
  Array_double *felicity = InitArray(double, {0.0, 1.5, 0.8});
  Array_double *survivor_ratios = InitArray(double, {0.8, 0.55});

  Matrix_double *m = InitMatrixWithSize(double, 3, 3, 0.0);
  m->data[0]->data[0] = 0.0;
  m->data[0]->data[1] = 1.5;
  m->data[0]->data[2] = 0.8;
  m->data[1]->data[0] = 0.8;
  m->data[2]->data[1] = 0.55;

  Matrix_double *leslie = leslie_matrix(survivor_ratios, felicity);

  EXPECT_TRUE(matrix_equal(leslie, m));

  free_matrix(leslie);
  free_matrix(m);
  free_vector(felicity);
  free_vector(survivor_ratios);
}
