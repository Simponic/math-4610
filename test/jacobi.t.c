#include "lizfcm.test.h"
#include <assert.h>
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

UTEST(jacobi, gauss_siedel_solve) {
  Matrix_double *m = generate_ddm(2);

  Array_double *b_1 = InitArrayWithSize(double, m->rows, 1.0);
  Array_double *b = m_dot_v(m, b_1);

  double tolerance = 0.001;
  size_t max_iter = 400;
  Array_double *solution = gauss_siedel_solve(m, b, tolerance, max_iter);

  for (size_t y = 0; y < m->rows; y++) {
    double dot = v_dot_v(m->data[y], solution);
    EXPECT_NEAR(b->data[y], dot, 0.1);
  }

  free_matrix(m);
  free_vector(b_1);
  free_vector(b);
  free_vector(solution);
}

UTEST(jacobi, leslie_solve) {
  Array_double *felicity = InitArray(double, {0.0, 1.5, 0.8});
  Array_double *survivor_ratios = InitArray(double, {0.8, 0.55});
  Matrix_double *leslie = leslie_matrix(survivor_ratios, felicity);

  Array_double *initial_pop = InitArray(double, {10.0, 20.0, 15.0});
  Array_double *next = m_dot_v(leslie, initial_pop);

  Matrix_double *augmented = add_column(leslie, next);
  Matrix_double *leslie_augmented_echelon = gaussian_elimination(augmented);

  Array_double *next_echelon =
      col_v(leslie_augmented_echelon, leslie_augmented_echelon->cols - 1);
  Matrix_double *leslie_echelon = slice_column(
      leslie_augmented_echelon, leslie_augmented_echelon->cols - 1);

  double tolerance = 0.001;
  size_t max_iter = 400;
  Array_double *initial_pop_guess =
      jacobi_solve(leslie_echelon, next_echelon, tolerance, max_iter);

  for (size_t y = 0; y < initial_pop->size; y++) {
    EXPECT_NEAR(initial_pop_guess->data[y], initial_pop->data[y], 0.05);
  }

  free_matrix(leslie);
  free_matrix(augmented);
  free_matrix(leslie_augmented_echelon);
  free_matrix(leslie_echelon);

  free_vector(felicity);
  free_vector(survivor_ratios);
  free_vector(next);
  free_vector(next_echelon);
  free_vector(initial_pop);
  free_vector(initial_pop_guess);
}
