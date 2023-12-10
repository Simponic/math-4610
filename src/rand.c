#include "lizfcm.h"

double rand_from(double min, double max) {
  return min + (rand() / (RAND_MAX / (max - min)));
}
