#include "lizfcm.test.h"
#include <math.h>

UTEST(vector, copy_vector) {
  Array_double *v = InitArray(double, {3, 1, -4});
  Array_double *w = copy_vector(v);
  EXPECT_TRUE(vector_equal(v, w));

  free_vector(v);
  free_vector(w);
}

UTEST(vector, free_vector) {
  Array_double *v = InitArray(double, {3, 1, -4});
  uint64_t arr_addr = (uint64_t)v->data;
  free_vector(v);
  EXPECT_NE((uint64_t)v->data, arr_addr);
}

UTEST(vector, sum_vector) {
  Array_double *v = InitArray(double, {3, 1, -4});
  EXPECT_EQ(0.0, sum_v(v));
  free_vector(v);
}

UTEST(vector, add_v) {
  Array_double *a = InitArray(double, {1.0, 3.0, -4.0});
  Array_double *b = InitArray(double, {2.0, -1.0, 0});
  Array_double *expected_sum = InitArray(double, {3.0, 2.0, -4.0});
  Array_double *sum = add_v(a, b);

  EXPECT_TRUE(vector_equal(sum, expected_sum));

  free_vector(a);
  free_vector(b);
  free_vector(expected_sum);
  free_vector(sum);
}

UTEST(vector, minus_v) {
  Array_double *a = InitArray(double, {1.0, 3.0, -4.0});
  Array_double *b = InitArray(double, {2.0, -1.0, 0});
  Array_double *expected_sub = InitArray(double, {-1.0, 4.0, -4.0});
  Array_double *sub = minus_v(a, b);

  EXPECT_TRUE(vector_equal(sub, expected_sub));

  free_vector(a);
  free_vector(b);
  free_vector(expected_sub);
  free_vector(sub);
}

UTEST(vector, scale_v) {
  double factor = 3.0;
  Array_double *a = InitArray(double, {1.0, 3.0, -4.0});
  Array_double *expected_scaled = InitArray(double, {3.0, 9.0, -12.0});
  Array_double *scaled = scale_v(a, factor);

  EXPECT_TRUE(vector_equal(scaled, expected_scaled));

  free_vector(a);
  free_vector(expected_scaled);
  free_vector(scaled);
}

UTEST(vector, l1_norm) {
  Array_double *v = InitArray(double, {3, 1, -4});
  EXPECT_EQ(l1_norm(v), 8.0);
  free_vector(v);
}

UTEST(vector, l2_norm) {
  Array_double *v = InitArray(double, {3, 1, -4});
  EXPECT_EQ(l2_norm(v), sqrt(3 * 3 + 1 * 1 + 4 * 4));
  free_vector(v);
}

UTEST(vector, linf_norm) {
  Array_double *v = InitArray(double, {3, 1, -4});
  EXPECT_EQ(linf_norm(v), c_max(c_max(3.0, 1.0), -4.0));
  free_vector(v);
}

UTEST(vector, vector_distance) {
  Array_double *v = InitArray(double, {3, 1, -4});
  Array_double *w = InitArray(double, {3, 1, -4});
  Array_double *minus = minus_v(v, w);
  EXPECT_EQ(vector_distance(v, w, &l2_norm), l2_norm(minus));
  free_vector(v);
  free_vector(w);
  free_vector(minus);
}
