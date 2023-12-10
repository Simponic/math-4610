#include "lizfcm.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

Array_double *m_dot_v(Matrix_double *m, Array_double *v) {
  assert(v->size == m->cols);

  Array_double *product = copy_vector(v);

  for (size_t row = 0; row < v->size; ++row)
    product->data[row] = v_dot_v(m->data[row], v);

  return product;
}

Array_double *col_v(Matrix_double *m, size_t x) {
  assert(x < m->cols);

  Array_double *col = InitArrayWithSize(double, m->rows, 0.0);
  for (size_t y = 0; y < m->rows; y++)
    col->data[y] = m->data[y]->data[x];

  return col;
}

Matrix_double *m_dot_m(Matrix_double *a, Matrix_double *b) {
  assert(a->cols == b->rows);

  Matrix_double *prod = InitMatrixWithSize(double, a->rows, b->cols, 0.0);

  Array_double *curr_col;
  for (size_t y = 0; y < a->rows; y++) {
    for (size_t x = 0; x < b->cols; x++) {
      curr_col = col_v(b, x);
      prod->data[y]->data[x] = v_dot_v(curr_col, a->data[y]);
      free_vector(curr_col);
    }
  }

  return prod;
}

Matrix_double *transpose(Matrix_double *m) {
  Matrix_double *transposed = InitMatrixWithSize(double, m->cols, m->rows, 0.0);

  for (size_t x = 0; x < m->rows; x++)
    for (size_t y = 0; y < m->cols; y++)
      transposed->data[y]->data[x] = m->data[x]->data[y];

  return transposed;
}

Matrix_double *put_identity_diagonal(Matrix_double *m) {
  assert(m->rows == m->cols);
  Matrix_double *copy = copy_matrix(m);
  for (size_t y = 0; y < m->rows; ++y)
    copy->data[y]->data[y] = 1.0;
  return copy;
}

Matrix_double *copy_matrix(Matrix_double *m) {
  Matrix_double *copy = InitMatrixWithSize(double, m->rows, m->cols, 0.0);
  for (size_t y = 0; y < copy->rows; y++) {
    free_vector(copy->data[y]);
    copy->data[y] = copy_vector(m->data[y]);
  }
  return copy;
}

Matrix_double **lu_decomp(Matrix_double *m) {
  assert(m->cols == m->rows);

  Matrix_double *u = copy_matrix(m);
  Matrix_double *l_empt = InitMatrixWithSize(double, m->rows, m->cols, 0.0);
  Matrix_double *l = put_identity_diagonal(l_empt);
  free_matrix(l_empt);

  Matrix_double **u_l = malloc(sizeof(Matrix_double *) * 2);

  for (size_t y = 0; y < m->rows; y++) {
    if (u->data[y]->data[y] == 0) {
      printf("ERROR: a pivot is zero in given matrix\n");
      assert(false);
    }
  }

  if (u && l) {
    for (size_t x = 0; x < m->cols; x++) {
      for (size_t y = x + 1; y < m->rows; y++) {
        double denom = u->data[x]->data[x];

        if (denom == 0) {
          printf("ERROR: non-factorable matrix\n");
          assert(false);
        }

        double factor = -(u->data[y]->data[x] / denom);

        Array_double *scaled = scale_v(u->data[x], factor);
        Array_double *added = add_v(scaled, u->data[y]);
        free_vector(scaled);
        free_vector(u->data[y]);

        u->data[y] = added;
        l->data[y]->data[x] = -factor;
      }
    }
  }

  u_l[0] = u;
  u_l[1] = l;
  return u_l;
}

Array_double *bsubst(Matrix_double *u, Array_double *b) {
  assert(u->rows == b->size && u->cols == u->rows);

  Array_double *x = copy_vector(b);
  for (int64_t row = b->size - 1; row >= 0; row--) {
    for (size_t col = b->size - 1; col > row; col--)
      x->data[row] -= x->data[col] * u->data[row]->data[col];
    x->data[row] /= u->data[row]->data[row];
  }
  return x;
}

Array_double *fsubst(Matrix_double *l, Array_double *b) {
  assert(l->rows == b->size && l->cols == l->rows);

  Array_double *x = copy_vector(b);

  for (size_t row = 0; row < b->size; row++) {
    for (size_t col = 0; col < row; col++)
      x->data[row] -= x->data[col] * l->data[row]->data[col];
    x->data[row] /= l->data[row]->data[row];
  }

  return x;
}

Array_double *solve_matrix_lu_bsubst(Matrix_double *m, Array_double *b) {
  assert(b->size == m->rows);
  assert(m->rows == m->cols);

  Array_double *x = copy_vector(b);
  Matrix_double **u_l = lu_decomp(m);
  Matrix_double *u = u_l[0];
  Matrix_double *l = u_l[1];

  Array_double *b_fsub = fsubst(l, b);
  x = bsubst(u, b_fsub);
  free_vector(b_fsub);

  free_matrix(u);
  free_matrix(l);
  free(u_l);

  return x;
}

Matrix_double *gaussian_elimination(Matrix_double *m) {
  uint64_t h = 0, k = 0;

  Matrix_double *m_cp = copy_matrix(m);

  while (h < m_cp->rows && k < m_cp->cols) {
    uint64_t max_row = h;
    double max_val = 0.0;

    for (uint64_t row = h; row < m_cp->rows; row++) {
      double val = fabs(m_cp->data[row]->data[k]);
      if (val > max_val) {
        max_val = val;
        max_row = row;
      }
    }

    if (max_val == 0.0) {
      k++;
      continue;
    }

    if (max_row != h) {
      Array_double *swp = m_cp->data[max_row];
      m_cp->data[max_row] = m_cp->data[h];
      m_cp->data[h] = swp;
    }

    for (uint64_t row = h + 1; row < m_cp->rows; row++) {
      double factor = m_cp->data[row]->data[k] / m_cp->data[h]->data[k];
      m_cp->data[row]->data[k] = 0.0;

      for (uint64_t col = k + 1; col < m_cp->cols; col++) {
        m_cp->data[row]->data[col] -= m_cp->data[h]->data[col] * factor;
      }
    }

    h++;
    k++;
  }

  return m_cp;
}

Array_double *solve_matrix_gaussian(Matrix_double *m, Array_double *b) {
  assert(b->size == m->rows);
  assert(m->rows == m->cols);

  Matrix_double *m_augment_b = add_column(m, b);
  Matrix_double *eliminated = gaussian_elimination(m_augment_b);

  Array_double *b_gauss = col_v(eliminated, m->cols);
  Matrix_double *u = slice_column(eliminated, m->rows);

  Array_double *solution = bsubst(u, b_gauss);

  free_matrix(m_augment_b);
  free_matrix(eliminated);
  free_matrix(u);
  free_vector(b_gauss);

  return solution;
}

Array_double *jacobi_solve(Matrix_double *m, Array_double *b,
                           double l2_convergence_tolerance,
                           size_t max_iterations) {
  assert(m->rows == m->cols);
  assert(b->size == m->cols);
  size_t iter = max_iterations;

  Array_double *x_k = InitArrayWithSize(double, b->size, 0.0);
  Array_double *x_k_1 =
      InitArrayWithSize(double, b->size, rand_from(0.1, 10.0));

  while ((--iter) > 0 && l2_distance(x_k_1, x_k) > l2_convergence_tolerance) {
    for (size_t i = 0; i < m->rows; i++) {
      double delta = 0.0;
      for (size_t j = 0; j < m->cols; j++) {
        if (i == j)
          continue;
        delta += m->data[i]->data[j] * x_k->data[j];
      }
      x_k_1->data[i] = (b->data[i] - delta) / m->data[i]->data[i];
    }

    Array_double *tmp = x_k;
    x_k = x_k_1;
    x_k_1 = tmp;
  }

  free_vector(x_k);
  return x_k_1;
}

Array_double *gauss_siedel_solve(Matrix_double *m, Array_double *b,
                                 double l2_convergence_tolerance,
                                 size_t max_iterations) {
  assert(m->rows == m->cols);
  assert(b->size == m->cols);
  size_t iter = max_iterations;

  Array_double *x_k = InitArrayWithSize(double, b->size, 0.0);
  Array_double *x_k_1 =
      InitArrayWithSize(double, b->size, rand_from(0.1, 10.0));

  while ((--iter) > 0) {
    for (size_t i = 0; i < x_k->size; i++)
      x_k->data[i] = x_k_1->data[i];

    for (size_t i = 0; i < m->rows; i++) {
      double delta = 0.0;
      for (size_t j = 0; j < m->cols; j++) {
        if (i == j)
          continue;
        delta += m->data[i]->data[j] * x_k_1->data[j];
      }
      x_k_1->data[i] = (b->data[i] - delta) / m->data[i]->data[i];
    }

    if (l2_distance(x_k_1, x_k) <= l2_convergence_tolerance)
      break;
  }

  free_vector(x_k);
  return x_k_1;
}

Matrix_double *slice_column(Matrix_double *m, size_t x) {
  Matrix_double *sliced = copy_matrix(m);

  for (size_t row = 0; row < m->rows; row++) {
    Array_double *old_row = sliced->data[row];
    sliced->data[row] = slice_element(old_row, x);
    free_vector(old_row);
  }
  sliced->cols--;

  return sliced;
}

Matrix_double *add_column(Matrix_double *m, Array_double *v) {
  Matrix_double *pushed = copy_matrix(m);

  for (size_t row = 0; row < m->rows; row++) {
    Array_double *old_row = pushed->data[row];
    pushed->data[row] = add_element(old_row, v->data[row]);
    free_vector(old_row);
  }

  pushed->cols++;
  return pushed;
}

void free_matrix(Matrix_double *m) {
  for (size_t y = 0; y < m->rows; ++y)
    free_vector(m->data[y]);
  free(m);
}

void format_matrix_into(Matrix_double *m, char *s) {
  if (m->rows == 0)
    strcpy(s, "empty");

  for (size_t y = 0; y < m->rows; ++y) {
    char row_s[5192];
    strcpy(row_s, "");

    format_vector_into(m->data[y], row_s);
    strcat(s, row_s);
  }
  strcat(s, "\n");
}

int matrix_equal(Matrix_double *a, Matrix_double *b) {
  if (a->cols != b->cols || a->rows != b->rows)
    return false;

  for (size_t y = 0; y < a->rows; ++y)
    if (!vector_equal(a->data[y], b->data[y])) {
      return false;
    }
  return true;
}
