#include "lizfcm.test.h"

UTEST(Lin, least_squares_lin_reg_perfect) {
  Array_double *x = InitArray(double, {0, 1, 2, 3, 4});
  Array_double *y = InitArray(double, {1, 2, 3, 4, 5});

  Line *line = least_squares_lin_reg(x, y);

  EXPECT_EQ(line->m, 1.0);
  EXPECT_EQ(line->a, 1.0);
}

UTEST(Lin, least_squares_lin_reg_estimate) {
  Array_double *x = InitArray(double, {1, 2, 3, 4, 5, 6, 7});
  Array_double *y = InitArray(double, {0.5, 3, 2, 3.5, 5, 6, 7.5});

  Line *line = least_squares_lin_reg(x, y);

  EXPECT_NEAR(line->m, 1.0, 0.2);
}
