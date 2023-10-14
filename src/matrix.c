#include "lizfcm.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

Array_double *m_dot_v(Matrix_double *m, Array_double *v) {
  assert(v->size == m->cols);

  Array_double *product = copy_vector(v);

  for (size_t row = 0; row < v->size; ++row)
    product->data[row] = v_dot_v(m->data[row], v);

  return product;
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
  Matrix_double *l = InitMatrixWithSize(double, m->rows, m->cols, 0.0);
  put_identity_diagonal(l);

  Matrix_double **u_l = malloc(sizeof(Matrix_double *) * 2);

  for (size_t y = 0; y < m->rows; y++) {
    if (u->data[y]->data[y] == 0) {
      printf("ERROR: a pivot is zero in given matrix\n");
      exit(-1);
    }
  }

  if (u && l) {
    for (size_t x = 0; x < m->cols; x++) {
      for (size_t y = x + 1; y < m->rows; y++) {
        double denom = u->data[x]->data[x];

        if (denom == 0) {
          printf("ERROR: non-factorable matrix\n");
          exit(-1);
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

Array_double *solve_matrix(Matrix_double *m, Array_double *b) {
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

  return x;
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
    char row_s[256];
    strcpy(row_s, "");

    format_vector_into(m->data[y], row_s);
    strcat(s, row_s);
  }
  strcat(s, "\n");
}
