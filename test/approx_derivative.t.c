#include "lizfcm.test.h"

double f(double x) { return x * x; }

double f_prime(double x) { return 2 * x; }

double H = 0.0001;
double ACCEPTED_DERIVATIVE_ERROR = 0.0001;

UTEST(derivative, central) {
  double a = 3.0;
  double expected = f_prime(a);
  double f_prime_x = central_derivative_at(&f, a, H);

  EXPECT_NEAR(expected, f_prime_x, ACCEPTED_DERIVATIVE_ERROR);
}

UTEST(derivative, forward) {
  double a = 3.0;
  double expected = f_prime(a);
  double f_prime_x = forward_derivative_at(&f, a, H);

  EXPECT_NEAR(expected, f_prime_x, ACCEPTED_DERIVATIVE_ERROR);
}

UTEST(derivative, backward) {
  double a = 3.0;
  double expected = f_prime(a);
  double f_prime_x = backward_derivative_at(&f, a, H);

  EXPECT_NEAR(expected, f_prime_x, ACCEPTED_DERIVATIVE_ERROR);
}
