#include "lizfcm.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>

double l2_norm(Array_double *v) {
  double norm = 0;
  for (size_t i = 0; i < v->size; ++i)
    norm += v->data[i] * v->data[i];
  return sqrt(norm);
}

double l1_norm(Array_double *v) {
  double sum = 0;
  for (size_t i = 0; i < v->size; ++i)
    sum += fabs(v->data[i]);
  return sum;
}

double linf_norm(Array_double *v) {
  double max = -DBL_MAX;
  for (size_t i = 0; i < v->size; ++i)
    max = c_max(v->data[i], max);
  return max;
}

Array_double *minus_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  Array_double *sub = InitArrayWithSize(double, v1->size, 0);
  for (size_t i = 0; i < v1->size; i++)
    sub->data[i] = v1->data[i] - v2->data[i];
  return sub;
}

double sum_v(Array_double *v) {
  double sum = 0;
  for (size_t i = 0; i < v->size; i++)
    sum += v->data[i];
  return sum;
}

Array_double *scale_v(Array_double *v, double m) {
  Array_double *copy = copy_vector(v);
  for (size_t i = 0; i < v->size; i++)
    copy->data[i] *= m;
  return copy;
}

Array_double *add_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  Array_double *sum = copy_vector(v1);
  for (size_t i = 0; i < v1->size; i++)
    sum->data[i] += v2->data[i];
  return sum;
}

double dot_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  double dot = 0;
  for (size_t i = 0; i < v1->size; i++)
    dot += v1->data[i] * v2->data[i];
  return dot;
}

double l2_distance(Array_double *v1, Array_double *v2) {
  Array_double *minus = minus_v(v1, v2);
  double dist = l2_norm(minus);
  free(minus);
  return dist;
}

double l1_distance(Array_double *v1, Array_double *v2) {
  Array_double *minus = minus_v(v1, v2);
  double dist = l1_norm(minus);
  free(minus);
  return dist;
}

double linf_distance(Array_double *v1, Array_double *v2) {
  Array_double *minus = minus_v(v1, v2);
  double dist = linf_norm(minus);
  free(minus);
  return dist;
}

Array_double *copy_vector(Array_double *v) {
  Array_double *copy = InitArrayWithSize(double, v->size, 0.0);
  for (size_t i = 0; i < copy->size; ++i)
    copy->data[i] = v->data[i];
  return copy;
}

void free_vector(Array_double *v) {
  free(v->data);
  free(v);
}

void format_vector_into(Array_double *v, char *s) {
  sprintf(s, "");
  if (v->size == 0)
    sprintf(s, "empty");

  for (size_t i = 0; i < v->size; ++i)
    sprintf(s, "%s %f,", s, v->data[i]);
}
