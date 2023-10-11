#include "lizfcm.h"
#include <math.h>

float smaceps() {
  float one = 1.0;
  float machine_epsilon = 1.0;
  float one_approx = one + machine_epsilon;

  while (fabsf(one_approx - one) > 0) {
    machine_epsilon /= 2;
    one_approx = one + machine_epsilon;
  }

  return machine_epsilon;
}

double dmaceps() {
  double one = 1.0;
  double machine_epsilon = 1.0;
  double one_approx = one + machine_epsilon;

  while (fabs(one_approx - one) > 0) {
    machine_epsilon /= 2;
    one_approx = one + machine_epsilon;
  }

  return machine_epsilon;
}
