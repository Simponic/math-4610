#include "lizfcm.test.h"
#include <math.h>
#include <stdio.h>

double f1(double x) { return x * x - 9; }

double f2(double x) { return x * x - 5 * x + 6; }
double f2prime(double x) { return 2 * x - 5; }
double g1(double x) { return x + f2(x); }
double g2(double x) { return x - f2(x); }

UTEST(ivt, find_interval) {
  Array_double *range = find_ivt_range(&f1, -10.0, 0.10, 200);
  EXPECT_LT(f1(range->data[0]) * f1(range->data[1]), 0);

  free_vector(range);
}

UTEST(root, bisection_with_error_assumption) {
  Array_double *range = find_ivt_range(&f2, 2.5, 0.10, 200);

  double tolerance = 0.01;
  double root1 = bisect_find_root_with_error_assumption(
      &f2, range->data[0], range->data[1], tolerance);

  free_vector(range);
  range = find_ivt_range(&f2, 0, 0.01, 500);
  double root2 = bisect_find_root_with_error_assumption(
      &f2, range->data[0], range->data[1], tolerance);
  free_vector(range);

  EXPECT_NEAR(3.0, root1, tolerance);
  EXPECT_NEAR(2.0, root2, tolerance);
}

UTEST(root, fixed_point_iteration_method) {
  // x^2 - 5x + 6 = (x - 3)(x - 2)
  double expect_x2 = 3.0;
  double expect_x1 = 2.0;

  double tolerance = 0.001;
  uint64_t max_iterations = 10;

  double x_0 = 1.55; // 1.5 < 1.55 < 2.5
  // g1(x) = x + f(x)
  double root1 =
      fixed_point_iteration_method(&f2, &g1, x_0, tolerance, max_iterations);
  EXPECT_NEAR(root1, expect_x1, tolerance);

  // g2(x) = x - f(x)
  x_0 = 3.4; // 2.5 < 3.4 < 3.5
  double root2 =
      fixed_point_iteration_method(&f2, &g2, x_0, tolerance, max_iterations);
  EXPECT_NEAR(root2, expect_x2, tolerance);
}

UTEST(root, fixed_point_newton_method) {
  // x^2 - 5x + 6 = (x - 3)(x - 2)
  double expect_x2 = 3.0;
  double expect_x1 = 2.0;

  double tolerance = 0.01;
  uint64_t max_iterations = 10;

  double x_0 = 1.55; // 1.5 < 1.55 < 2.5
  double root1 =
      fixed_point_newton_method(&f2, &f2prime, x_0, tolerance, max_iterations);
  EXPECT_NEAR(root1, expect_x1, tolerance);

  x_0 = 3.4; // 2.5 < 3.4 < 3.5
  double root2 =
      fixed_point_newton_method(&f2, &f2prime, x_0, tolerance, max_iterations);
  EXPECT_NEAR(root2, expect_x2, tolerance);
}

UTEST(root, fixed_point_secant_method) {
  // x^2 - 5x + 6 = (x - 3)(x - 2)
  double expect_x2 = 3.0;
  double expect_x1 = 2.0;

  double delta = 0.01;
  double tolerance = 0.01;
  uint64_t max_iterations = 10;

  double x_0 = 1.55; // 1.5 < 1.55 < 2.5
  double root1 = fixed_point_secant_method(&f2, x_0, x_0 + delta, tolerance,
                                           max_iterations);
  EXPECT_NEAR(root1, expect_x1, tolerance);

  x_0 = 3.4; // 2.5 < 3.4 < 3.5
  double root2 = fixed_point_secant_method(&f2, x_0, x_0 + delta, tolerance,
                                           max_iterations);
  EXPECT_NEAR(root2, expect_x2, tolerance);
}

UTEST(root, fixed_point_hybrid_method) {
  // x^2 - 5x + 6 = (x - 3)(x - 2)
  double expect_x2 = 3.0;
  double expect_x1 = 2.0;

  double delta = 1.0;
  double tolerance = 0.01;
  uint64_t max_iterations = 10;

  double x_0 = 1.55;
  double root1 = fixed_point_secant_bisection_method(&f2, x_0, x_0 + delta,
                                                     tolerance, max_iterations);
  EXPECT_NEAR(root1, expect_x1, tolerance);

  x_0 = 2.5;
  double root2 = fixed_point_secant_bisection_method(&f2, x_0, x_0 + delta,
                                                     tolerance, max_iterations);
  EXPECT_NEAR(root2, expect_x2, tolerance);
}
