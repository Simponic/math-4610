#include "macros.h"
#include <stddef.h>

#ifndef TYPES_H
#define TYPES_H

DEFINE_ARRAY(int);
DEFINE_ARRAY(uint32_t);
DEFINE_ARRAY(int32_t);
DEFINE_ARRAY(float);
DEFINE_ARRAY(double);

typedef struct Line {
  double m;
  double a;
} Line;

#endif // TYPES_H
