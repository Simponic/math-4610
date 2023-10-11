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

#define InitArray(TYPE, ...)                                                   \
  ({                                                                           \
    TYPE temp[] = __VA_ARGS__;                                                 \
    Array_##TYPE *arr = malloc(sizeof(Array_##TYPE));                          \
    arr->size = sizeof(temp) / sizeof(temp[0]);                                \
    arr->data = malloc(arr->size * sizeof(TYPE));                              \
    if (arr->data) {                                                           \
      memcpy(arr->data, temp, arr->size * sizeof(TYPE));                       \
    }                                                                          \
    arr;                                                                       \
  })

#define InitArrayWithSize(TYPE, SIZE, INIT_VALUE)                              \
  ({                                                                           \
    Array_##TYPE *arr = malloc(sizeof(Array_##TYPE));                          \
    arr->size = SIZE;                                                          \
    arr->data = malloc(arr->size * sizeof(TYPE));                              \
    if (arr->data) {                                                           \
      for (size_t i = 0; i < arr->size; i++)                                   \
        arr->data[i] = INIT_VALUE;                                             \
    }                                                                          \
    arr;                                                                       \
  })

#define c_max(x, y) (((x) >= (y)) ? (x) : (y))
#define c_min(x, y) (((x) <= (y)) ? (x) : (y))

#endif // MACROS_H
