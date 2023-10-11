#include "lizfcm.h"
#include <assert.h>
#include <stdio.h>

void put_identity_diagonal(Matrix_double *m) {
  assert(m->rows == m->cols);

  for (size_t y = 0; y < m->rows; ++y)
    m->data[y]->data[y] = 1.0;
}

void format_matrix_into(Matrix_double *m, char *s) {
  sprintf(s, "");
  if (m->rows == 0)
    sprintf(s, "empty");

  for (size_t y = 0; y < m->rows; ++y) {
    char row_s[256];
    format_vector_into(m->data[y], row_s);
    sprintf(s, "%s %s \n", s, row_s);
  }
}
