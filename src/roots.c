#include "lizfcm.h"
#include <assert.h>
#include <math.h>

// f is well defined at start_x + delta*n for all n on the integer range [0,
// max_steps]
double *find_ivt_range(double (*f)(double), double start_x, double delta,
                       size_t max_steps) {
  double *range = malloc(sizeof(double) * 2);

  double a = start_x;

  while (f(a) * f(start_x) >= 0 && max_steps-- > 0)
    a += delta;

  if (max_steps == 0 && f(a) * f(start_x) > 0)
    return NULL;

  range[0] = start_x;
  range[1] = a + delta;
  return range;
}

// f is continuous on [a, b]
double bisect_find_root(double (*f)(double), double a, double b,
                        double tolerance, size_t max_iterations) {
  assert(a <= b);
  // guarantee there's a root somewhere between a and b by IVT
  assert(f(a) * f(b) < 0);

  double c = (1.0 / 2) * (a + b);
  if (b - a < tolerance || max_iterations == 0)
    return c;
  if (f(a) * f(c) < 0)
    return bisect_find_root(f, a, c, tolerance, max_iterations - 1);
  return bisect_find_root(f, c, b, tolerance, max_iterations - 1);
}

double bisect_find_root_with_error_assumption(double (*f)(double), double a,
                                              double b, double tolerance) {
  assert(a <= b);

  uint64_t max_iterations =
      (uint64_t)ceil((log(tolerance) - log(b - a)) / log(1 / 2.0));
  return bisect_find_root(f, a, b, tolerance, max_iterations);
}
