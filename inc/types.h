#include "macros.h"
#include <stddef.h>

#ifndef TYPES_H
#define TYPES_H

DEFINE_ARRAY(int);
DEFINE_ARRAY(uint32_t);
DEFINE_ARRAY(int32_t);
DEFINE_ARRAY(float);
DEFINE_ARRAY(double);

DEFINE_MATRIX(int);
DEFINE_MATRIX(uint32_t);
DEFINE_MATRIX(int32_t);
DEFINE_MATRIX(float);
DEFINE_MATRIX(double);

typedef struct Line {
  double m;
  double a;
} Line;

#endif // TYPES_H
