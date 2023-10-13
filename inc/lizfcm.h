#include "macros.h"
#include "types.h"

#ifndef LIZFCM_H
#define LIZFCM_H

extern float smaceps();
extern double dmaceps();

extern double central_derivative_at(double (*f)(double), double a, double h);
extern double forward_derivative_at(double (*f)(double), double a, double h);
extern double backward_derivative_at(double (*f)(double), double a, double h);

extern double sum_v(Array_double *v);
extern Array_double *scale_v(Array_double *v1, double m);
extern Array_double *add_v(Array_double *v1, Array_double *v2);
extern Array_double *minus_v(Array_double *v1, Array_double *v2);
extern double dot_v(Array_double *v1, Array_double *v2);
extern double l2_norm(Array_double *v);
extern double l1_norm(Array_double *v);
extern double linf_norm(Array_double *v);
extern double l2_distance(Array_double *v1, Array_double *v2);
extern double l1_distance(Array_double *v1, Array_double *v2);
extern double linf_distance(Array_double *v1, Array_double *v2);
extern Array_double *copy_vector(Array_double *v1);
extern void free_vector(Array_double *v);
extern void format_vector_into(Array_double *v, char *s);
extern Line *least_squares_lin_reg(Array_double *x, Array_double *y);

extern void put_identity_diagonal(Matrix_double *m);
extern Matrix_double *copy_matrix(Matrix_double *m);
extern void free_matrix(Matrix_double *m);
extern Matrix_double **lu_decomp(Matrix_double *m);
extern Array_double *bsubst(Matrix_double *u, Array_double *b);
extern Array_double *fsubst(Matrix_double *l, Array_double *b);

extern void format_matrix_into(Matrix_double *m, char *s);

#endif // LIZFCM_H
