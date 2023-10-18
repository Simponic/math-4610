#include "lizfcm.test.h"
#include <stdio.h>

double g(double x) { return x * x - 9; }

UTEST(ivt, find_interval) {
  double *range = find_ivt_range(&g, -100.0, 1.0, 200);
  EXPECT_LT(g(range[0]) * g(range[1]), 0);

  free(range);
}

UTEST(root, bisection_with_error_assumption) {
  double root = bisect_find_root_with_error_assumption(&g, -5, 0, 0.01);

  EXPECT_NEAR(-3, root, 0.01);
}
