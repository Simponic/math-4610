// compile & test w/
//  \--> gcc -I../inc/ -Wall hw_6_p_8.c ../lib/lizfcm.a -lm -o hw_6_p_8
//  \--> ./hw_6_p_8

#include "lizfcm.h"
#include <math.h>
#include <stdio.h>

double a(double t) {
  double alpha = 0.1;
  double beta = 0.001;
  double p_0 = 2;
  double p_infty = 29.75;

  return p_0 * exp(t * (alpha - beta)) - p_infty;
}

double b(double t) {
  double alpha = 0.1;
  double beta = 0.001;
  double p_0 = 2;
  double p_infty = 115.35;

  return p_0 * exp(t * (alpha - beta)) - p_infty;
}

double c(double t) {
  double alpha = 0.1;
  double beta = 0.0001;
  double p_0 = 2;
  double p_infty = 115.35;

  return p_0 * exp(t * (alpha - beta)) - p_infty;
}

double d(double t) {
  double alpha = 0.01;
  double beta = 0.001;
  double p_0 = 2;
  double p_infty = 155.346;

  return p_0 * exp(t * (alpha - beta)) - p_infty;
}

double e(double t) {
  double alpha = 0.1;
  double beta = 0.01;
  double p_0 = 100;
  double p_infty = 155.346;

  return p_0 * exp(t * (alpha - beta)) - p_infty;
}

int main() {
  uint64_t max_iterations = 1000;
  double tolerance = 0.0000001;

  Array_double *ivt_range = find_ivt_range(&a, -5.0, 3.0, 1000);
  double approx_a = fixed_point_secant_bisection_method(
      &a, ivt_range->data[0], ivt_range->data[1], tolerance, max_iterations);

  free_vector(ivt_range);
  ivt_range = find_ivt_range(&b, -5.0, 3.0, 1000);
  double approx_b = fixed_point_secant_bisection_method(
      &b, ivt_range->data[0], ivt_range->data[1], tolerance, max_iterations);

  free_vector(ivt_range);
  ivt_range = find_ivt_range(&c, -5.0, 3.0, 1000);
  double approx_c = fixed_point_secant_bisection_method(
      &c, ivt_range->data[0], ivt_range->data[1], tolerance, max_iterations);

  free_vector(ivt_range);
  ivt_range = find_ivt_range(&d, -5.0, 3.0, 1000);
  double approx_d = fixed_point_secant_bisection_method(
      &d, ivt_range->data[0], ivt_range->data[1], tolerance, max_iterations);

  free_vector(ivt_range);
  ivt_range = find_ivt_range(&e, -5.0, 3.0, 1000);
  double approx_e = fixed_point_secant_bisection_method(
      &e, ivt_range->data[0], ivt_range->data[1], tolerance, max_iterations);

  printf("a ~ %f; P(%f) - P_infty = %f\n", approx_a, approx_a, a(approx_a));
  printf("b ~ %f; P(%f) - P_infty = %f\n", approx_b, approx_b, b(approx_b));
  printf("c ~ %f; P(%f) - P_infty = %f\n", approx_c, approx_c, c(approx_c));
  printf("d ~ %f; P(%f) - P_infty = %f\n", approx_d, approx_d, d(approx_d));
  printf("e ~ %f; P(%f) - P_infty = %f\n", approx_e, approx_e, e(approx_e));

  return 0;
}
