#include "lizfcm.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

Matrix_double *leslie_matrix(Array_double *age_class_surivor_ratio,
                             Array_double *age_class_offspring) {
  assert(age_class_surivor_ratio->size + 1 == age_class_offspring->size);

  Matrix_double *leslie = InitMatrixWithSize(double, age_class_offspring->size,
                                             age_class_offspring->size, 0.0);

  free_vector(leslie->data[0]);
  leslie->data[0] = copy_vector(age_class_offspring);

  for (size_t i = 0; i < age_class_surivor_ratio->size; i++)
    leslie->data[i + 1]->data[i] = age_class_surivor_ratio->data[i];
  return leslie;
}

double dominant_eigenvalue(Matrix_double *m, Array_double *v, double tolerance,
                           size_t max_iterations) {
  assert(m->rows == m->cols);
  assert(m->rows == v->size);

  double error = tolerance;
  size_t iter = max_iterations;
  double lambda = 0.0;
  Array_double *eigenvector_1 = copy_vector(v);

  while (error >= tolerance && (--iter) > 0) {
    Array_double *eigenvector_2 = m_dot_v(m, eigenvector_1);
    Array_double *normalized_eigenvector_2 =
        scale_v(eigenvector_2, 1.0 / linf_norm(eigenvector_2));
    free_vector(eigenvector_2);
    eigenvector_2 = normalized_eigenvector_2;

    Array_double *mx = m_dot_v(m, eigenvector_2);
    double new_lambda =
        v_dot_v(mx, eigenvector_2) / v_dot_v(eigenvector_2, eigenvector_2);

    error = fabs(new_lambda - lambda);
    lambda = new_lambda;
    free_vector(eigenvector_1);
    eigenvector_1 = eigenvector_2;
  }

  return lambda;
}

double least_dominant_eigenvalue(Matrix_double *m, Array_double *v,
                                 double tolerance, size_t max_iterations) {
  assert(m->rows == m->cols);
  assert(m->rows == v->size);

  double shift = 0.0;
  Matrix_double *m_c = copy_matrix(m);
  for (size_t y = 0; y < m_c->rows; ++y)
    m_c->data[y]->data[y] = m_c->data[y]->data[y] - shift;

  double error = tolerance;
  size_t iter = max_iterations;
  double lambda = shift;
  Array_double *eigenvector_1 = copy_vector(v);

  while (error >= tolerance && (--iter) > 0) {
    Array_double *eigenvector_2 = solve_matrix_lu_bsubst(m_c, eigenvector_1);
    Array_double *normalized_eigenvector_2 =
        scale_v(eigenvector_2, 1.0 / linf_norm(eigenvector_2));
    free_vector(eigenvector_2);
    eigenvector_2 = normalized_eigenvector_2;

    Array_double *mx = m_dot_v(m, eigenvector_2);
    double new_lambda =
        v_dot_v(mx, eigenvector_2) / v_dot_v(eigenvector_2, eigenvector_2);

    error = fabs(new_lambda - lambda);
    lambda = new_lambda;
    free_vector(eigenvector_1);
    eigenvector_1 = eigenvector_2;
  }

  return lambda;
}
