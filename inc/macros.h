#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifndef MACROS_H
#define MACROS_H

#define DEFINE_ARRAY(TYPE)                                                     \
  typedef struct {                                                             \
    TYPE *data;                                                                \
    size_t size;                                                               \
  } Array_##TYPE

#define DEFINE_MATRIX(TYPE)                                                    \
  typedef struct {                                                             \
    Array_##TYPE **data;                                                       \
    size_t cols;                                                               \
    size_t rows;                                                               \
  } Matrix_##TYPE

#define InitArray(TYPE, ...)                                                   \
  ({                                                                           \
    TYPE temp[] = __VA_ARGS__;                                                 \
    Array_##TYPE *arr = malloc(sizeof(Array_##TYPE));                          \
    arr->size = sizeof(temp) / sizeof(temp[0]);                                \
    arr->data = malloc(arr->size * sizeof(TYPE));                              \
    memcpy(arr->data, temp, arr->size * sizeof(TYPE));                         \
    arr;                                                                       \
  })

#define InitArrayWithSize(TYPE, SIZE, INIT_VALUE)                              \
  ({                                                                           \
    Array_##TYPE *arr = malloc(sizeof(Array_##TYPE));                          \
    arr->size = SIZE;                                                          \
    arr->data = malloc(arr->size * sizeof(TYPE));                              \
    for (size_t i = 0; i < arr->size; i++)                                     \
      arr->data[i] = INIT_VALUE;                                               \
    arr;                                                                       \
  })

#define InitMatrixWithSize(TYPE, ROWS, COLS, INIT_VALUE)                       \
  ({                                                                           \
    Matrix_##TYPE *matrix = malloc(sizeof(Matrix_##TYPE));                     \
    matrix->rows = ROWS;                                                       \
    matrix->cols = COLS;                                                       \
    matrix->data = malloc(matrix->rows * sizeof(Array_##TYPE *));              \
    for (size_t y = 0; y < matrix->rows; y++)                                  \
      matrix->data[y] = InitArrayWithSize(TYPE, COLS, INIT_VALUE);             \
    matrix;                                                                    \
  })

#define c_max(x, y) (((x) >= (y)) ? (x) : (y))
#define c_min(x, y) (((x) <= (y)) ? (x) : (y))

#define true 1
#define false 0

#endif // MACROS_H
