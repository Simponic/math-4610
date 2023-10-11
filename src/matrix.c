#include "lizfcm.h"
#include <assert.h>
#include <string.h>
#include <stdio.h>

void put_identity_diagonal(Matrix_double *m) {
  assert(m->rows == m->cols);

  for (size_t y = 0; y < m->rows; ++y)
    m->data[y]->data[y] = 1.0;
}

Matrix_double *copy_matrix(Matrix_double *m) {
  Matrix_double *copy = InitMatrixWithSize(double, m->rows, m->cols, 0.0);
  for (size_t y = 0; y < copy->rows; y++) {
    free_vector(copy->data[y]);
    copy->data[y] = copy_vector(m->data[y]);
  }
  return copy;
}

Matrix_double **put_lu_decomp(Matrix_double *m) {
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
