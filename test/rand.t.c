#include "lizfcm.test.h"

UTEST(rand, rand_from) {
  double min = -2.0;
  double max = 5.0;
  for (size_t i = 0; i < 1000; i++) {
    double r = rand_from(min, max);
    ASSERT_TRUE(min <= r && r <= max);
  }
}
