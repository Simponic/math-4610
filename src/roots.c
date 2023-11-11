#include "lizfcm.h"
#include <assert.h>
#include <math.h>

// f is well defined at start_x + delta*n for all n on the integer range [0,
// max_iterations]
Array_double *find_ivt_range(double (*f)(double), double start_x, double delta,
                             size_t max_iterations) {
  double a = start_x;

  while (f(a) * f(a + delta) >= 0 && max_iterations > 0) {
    max_iterations--;
    a += delta;
  }

  double end = a + delta;
  double begin = a - delta;

  if (max_iterations == 0 && f(begin) * f(end) >= 0)
    return NULL;
  return InitArray(double, {begin, end});
}

// f is continuous on [a, b]
Array_double *bisect_find_root(double (*f)(double), double a, double b,
                               double tolerance, size_t max_iterations) {
  assert(a <= b);
  // guarantee there's a root somewhere between a and b by IVT
  assert(f(a) * f(b) < 0);

  double c = (1.0 / 2) * (a + b);
  if (b - a < tolerance || max_iterations == 0)
    return InitArray(double, {a, b, c});

  if (f(a) * f(c) < 0)
    return bisect_find_root(f, a, c, tolerance, max_iterations - 1);
  return bisect_find_root(f, c, b, tolerance, max_iterations - 1);
}

double bisect_find_root_with_error_assumption(double (*f)(double), double a,
                                              double b, double tolerance) {
  assert(a <= b);

  uint64_t max_iterations =
      (uint64_t)ceil((log(tolerance) - log(b - a)) / log(1 / 2.0));

  Array_double *a_b_root = bisect_find_root(f, a, b, tolerance, max_iterations);
  double root = a_b_root->data[2];
  free_vector(a_b_root);

  return root;
}

double fixed_point_iteration_method(double (*f)(double), double (*g)(double),
                                    double x_0, double tolerance,
                                    size_t max_iterations) {
  if (max_iterations <= 0)
    return x_0;

  double root = g(x_0);
  if (tolerance >= fabs(f(root)))
    return root;

  return fixed_point_iteration_method(f, g, root, tolerance,
                                      max_iterations - 1);
}

double fixed_point_newton_method(double (*f)(double), double (*fprime)(double),
                                 double x_0, double tolerance,
                                 size_t max_iterations) {
  if (max_iterations <= 0)
    return x_0;

  double root = x_0 - f(x_0) / fprime(x_0);
  if (tolerance >= fabs(f(root)))
    return root;

  return fixed_point_newton_method(f, fprime, root, tolerance,
                                   max_iterations - 1);
}

double fixed_point_secant_method(double (*f)(double), double x_0, double x_1,
                                 double tolerance, size_t max_iterations) {
  if (max_iterations == 0)
    return x_1;

  double root = x_1 - f(x_1) * ((x_1 - x_0) / (f(x_1) - f(x_0)));

  if (tolerance >= fabs(f(root)))
    return root;

  return fixed_point_secant_method(f, x_1, root, tolerance, max_iterations - 1);
}

double fixed_point_secant_bisection_method(double (*f)(double), double x_0,
                                           double x_1, double tolerance,
                                           size_t max_iterations) {
  double begin = x_0;
  double end = x_1;
  double root = x_0;

  while (tolerance < fabs(f(root)) && max_iterations > 0) {
    max_iterations--;

    double secant_root = fixed_point_secant_method(f, begin, end, tolerance, 1);

    if (secant_root < begin || secant_root > end) {
      Array_double *range_root = bisect_find_root(f, begin, end, tolerance, 1);

      begin = range_root->data[0];
      end = range_root->data[1];
      root = range_root->data[2];

      free_vector(range_root);
      continue;
    }

    root = secant_root;

    if (f(root) * f(begin) < 0)
      end = secant_root; // the root exists in [begin, secant_root]
    else
      begin = secant_root;
  }

  return root;
}
