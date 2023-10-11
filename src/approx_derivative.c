#include "lizfcm.h"
#include <assert.h>

double central_derivative_at(double (*f)(double), double a, double h) {
  assert(h > 0);

  double x2 = a + h;
  double x1 = a - h;

  double y2 = (*f)(x2);
  double y1 = (*f)(x1);

  return (y2 - y1) / (x2 - x1);
}

double forward_derivative_at(double (*f)(double), double a, double h) {
  assert(h > 0);

  double x2 = a + h;
  double x1 = a;

  double y2 = (*f)(x2);
  double y1 = (*f)(x1);

  return (y2 - y1) / (x2 - x1);
}

double backward_derivative_at(double (*f)(double), double a, double h) {
  assert(h > 0);

  double x2 = a;
  double x1 = a - h;

  double y2 = (*f)(x2);
  double y1 = (*f)(x1);

  return (y2 - y1) / (x2 - x1);
}
