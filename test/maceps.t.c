#include "lizfcm.test.h"
#include <math.h>

UTEST(maceps, smaceps) {
  float epsilon = smaceps();
  float one = 1.0;
  float approx_one = one + epsilon;

  EXPECT_LE(approx_one - one, 0);
}

UTEST(maceps, dmaceps) {
  double epsilon = dmaceps();
  double one = 1.0;
  double approx_one = one + epsilon;

  EXPECT_LE(approx_one - one, 0);
}
