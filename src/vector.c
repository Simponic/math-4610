#include "lizfcm.h"
#include <assert.h>
#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>

Array_double *add_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  Array_double *sum = copy_vector(v1);
  for (size_t i = 0; i < v1->size; i++)
    sum->data[i] += v2->data[i];
  return sum;
}

Array_double *minus_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  Array_double *sub = InitArrayWithSize(double, v1->size, 0);
  for (size_t i = 0; i < v1->size; i++)
    sub->data[i] = v1->data[i] - v2->data[i];
  return sub;
}

Array_double *scale_v(Array_double *v, double m) {
  Array_double *copy = copy_vector(v);
  for (size_t i = 0; i < v->size; i++)
    copy->data[i] *= m;
  return copy;
}

double l1_norm(Array_double *v) {
  double sum = 0;
  for (size_t i = 0; i < v->size; ++i)
    sum += fabs(v->data[i]);
  return sum;
}

double l2_norm(Array_double *v) {
  double norm = 0;
  for (size_t i = 0; i < v->size; ++i)
    norm += v->data[i] * v->data[i];
  return sqrt(norm);
}

double linf_norm(Array_double *v) {
  assert(v->size > 0);
  double max = v->data[0];
  for (size_t i = 0; i < v->size; ++i)
    max = c_max(v->data[i], max);
  return max;
}

double v_dot_v(Array_double *v1, Array_double *v2) {
  assert(v1->size == v2->size);

  double dot = 0;
  for (size_t i = 0; i < v1->size; i++)
    dot += v1->data[i] * v2->data[i];
  return dot;
}

double vector_distance(Array_double *v1, Array_double *v2,
                       double (*norm)(Array_double *)) {
  Array_double *minus = minus_v(v1, v2);
  double dist = (*norm)(minus);
  free(minus);
  return dist;
}

double l1_distance(Array_double *v1, Array_double *v2) {
  return vector_distance(v1, v2, &l1_norm);
}

double l2_distance(Array_double *v1, Array_double *v2) {
  return vector_distance(v1, v2, &l2_norm);
}

double linf_distance(Array_double *v1, Array_double *v2) {
  return vector_distance(v1, v2, &linf_norm);
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
  if (v->size == 0) {
    strcat(s, "empty");
    return;
  }

  for (size_t i = 0; i < v->size; ++i) {
    char num[64];
    strcpy(num, "");

    sprintf(num, "%f,", v->data[i]);
    strcat(s, num);
  }
  strcat(s, "\n");
}

double sum_v(Array_double *v) {
  double sum = 0;
  for (size_t i = 0; i < v->size; i++)
    sum += v->data[i];
  return sum;
}

int vector_equal(Array_double *a, Array_double *b) {
  if (a->size != b->size)
    return false;

  for (size_t i = 0; i < a->size; ++i) {
    if (a->data[i] != b->data[i])
      return false;
  }
  return true;
}
