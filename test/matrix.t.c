#include "lizfcm.test.h"

UTEST(matrix, free) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  uint64_t data_addr = (uint64_t)(m->data);
  free_matrix(m);
  EXPECT_NE(data_addr, (uint64_t)(m->data));
}

UTEST(matrix, put_identity_diagonal) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  Matrix_double *ident = put_identity_diagonal(m);

  for (size_t y = 0; y < m->rows; ++y)
    for (size_t x = 0; x < m->cols; ++x)
      EXPECT_EQ(ident->data[y]->data[x], x == y ? 1.0 : 0.0);

  free_matrix(m);
  free_matrix(ident);
}

UTEST(matrix, copy) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  Matrix_double *ident = put_identity_diagonal(m);

  Matrix_double *copy = copy_matrix(ident);

  EXPECT_TRUE(matrix_equal(ident, copy));

  free_matrix(m);
  free_matrix(ident);
  free_matrix(copy);
}

UTEST(matrix, m_dot_v) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  Matrix_double *ident = put_identity_diagonal(m);

  Array_double *x = InitArray(double, {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0});
  Array_double *dotted = m_dot_v(ident, x);

  EXPECT_TRUE(vector_equal(dotted, x));

  free_matrix(m);
  free_matrix(ident);
  free_vector(x);
  free_vector(dotted);
}

UTEST(matrix, lu_decomp) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  for (size_t y = 0; y < m->rows; ++y) {
    for (size_t x = 0; x < m->cols; ++x)
      m->data[y]->data[x] = x == y ? 5.0 : (5.0 - rand() % 10 + 1);
  }

  Matrix_double **ul = lu_decomp(m);
  Matrix_double *u = ul[0];
  Matrix_double *l = ul[1];
  for (int y = 0; y < m->rows; y++) {
    for (size_t x = 0; x < c_max(y - 1, 0); x++) {
      double u_yx = u->data[y]->data[x];
      EXPECT_NEAR(u_yx, 0.0, 0.0001);
    }

    for (size_t x = c_min(m->cols, y + 1); x < m->cols; ++x) {
      double l_yx = l->data[y]->data[x];
      EXPECT_NEAR(l_yx, 0.0, 0.0001);
    }
  }

  free_matrix(m);
  free_matrix(l);
  free_matrix(u);
  free(ul);
}

UTEST(matrix, solve_matrix) {
  Matrix_double *m = InitMatrixWithSize(double, 8, 8, 0.0);
  for (size_t y = 0; y < m->rows; ++y) {
    for (size_t x = 0; x < m->cols; ++x)
      m->data[y]->data[x] = x == y ? 10.0 : (5.0 - rand() % 10 + 1);
  }

  Array_double *b = InitArray(double, {1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0});
  Array_double *solution = solve_matrix(m, b);

  for (size_t y = 0; y < m->rows; y++) {
    double dot = v_dot_v(m->data[y], solution);
    EXPECT_NEAR(b->data[y], dot, 0.0001);
  }

  free_matrix(m);
  free_vector(b);
  free_vector(solution);
}

UTEST(matrix, col_v) {
  Matrix_double *m = InitMatrixWithSize(double, 2, 3, 0.0);
  // set element to its column index
  for (size_t y = 0; y < m->rows; y++) {
    for (size_t x = 0; x < m->cols; x++) {
      m->data[y]->data[x] = x;
    }
  }

  Array_double *col, *expected;
  for (size_t x = 0; x < m->cols; x++) {
    col = col_v(m, x);
    expected = InitArrayWithSize(double, m->rows, (double)x);
    EXPECT_TRUE(vector_equal(expected, col));
    free_vector(col);
    free_vector(expected);
  }

  free_matrix(m);
}

UTEST(matrix, m_dot_m) {
  Matrix_double *a = InitMatrixWithSize(double, 1, 3, 12.0);
  Matrix_double *b = InitMatrixWithSize(double, 3, 1, 10.0);

  Matrix_double *prod = m_dot_m(a, b);

  EXPECT_EQ(prod->cols, 1);
  EXPECT_EQ(prod->rows, 1);
  EXPECT_EQ(12.0 * 10.0 * 3, prod->data[0]->data[0]);

  free_matrix(a);
  free_matrix(b);
  free_matrix(prod);
}
