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
extern double v_dot_v(Array_double *v1, Array_double *v2);
extern double l2_norm(Array_double *v);
extern double l1_norm(Array_double *v);
extern double linf_norm(Array_double *v);
extern double vector_distance(Array_double *v1, Array_double *v2,
                              double (*norm)(Array_double *));
extern double l2_distance(Array_double *v1, Array_double *v2);
extern double l1_distance(Array_double *v1, Array_double *v2);
extern double linf_distance(Array_double *v1, Array_double *v2);
extern Array_double *copy_vector(Array_double *v1);
extern Array_double *add_element(Array_double *v, double x);
extern Array_double *slice_element(Array_double *v, size_t x);
extern void free_vector(Array_double *v);
extern void format_vector_into(Array_double *v, char *s);
extern int vector_equal(Array_double *a, Array_double *b);

extern Matrix_double *put_identity_diagonal(Matrix_double *m);
extern Matrix_double **lu_decomp(Matrix_double *m);
extern Array_double *bsubst(Matrix_double *u, Array_double *b);
extern Array_double *fsubst(Matrix_double *l, Array_double *b);
extern Array_double *solve_matrix_lu_bsubst(Matrix_double *m, Array_double *b);
extern Matrix_double *gaussian_elimination(Matrix_double *m);
extern Array_double *solve_matrix_gaussian(Matrix_double *m, Array_double *b);
extern Array_double *m_dot_v(Matrix_double *m, Array_double *v);
extern Matrix_double *m_dot_m(Matrix_double *a, Matrix_double *b);
extern Matrix_double *transpose(Matrix_double *m);
extern Array_double *col_v(Matrix_double *m, size_t x);
extern Matrix_double *copy_matrix(Matrix_double *m);
extern Matrix_double *add_column(Matrix_double *m, Array_double *col);
extern Matrix_double *slice_column(Matrix_double *m, size_t col);
extern void free_matrix(Matrix_double *m);
extern void format_matrix_into(Matrix_double *m, char *s);
extern int matrix_equal(Matrix_double *a, Matrix_double *b);

extern Line *least_squares_lin_reg(Array_double *x, Array_double *y);

extern Array_double *find_ivt_range(double (*f)(double), double start_x,
                                    double delta, size_t max_steps);
extern Array_double *bisect_find_root(double (*f)(double), double a, double b,
                                      double tolerance, size_t max_iterations);
extern double bisect_find_root_with_error_assumption(double (*f)(double),
                                                     double a, double b,
                                                     double tolerance);
extern double fixed_point_iteration_method(double (*f)(double),
                                           double (*g)(double), double x_0,
                                           double tolerance,
                                           size_t max_iterations);
extern double fixed_point_newton_method(double (*f)(double),
                                        double (*fprime)(double), double x_0,
                                        double tolerance,
                                        size_t max_iterations);
extern double fixed_point_secant_method(double (*f)(double), double x_0,
                                        double x_1, double tolerance,
                                        size_t max_iterations);
extern double fixed_point_secant_bisection_method(double (*f)(double),
                                                  double x_0, double x_1,
                                                  double tolerance,
                                                  size_t max_iterations);

extern double dominant_eigenvalue(Matrix_double *m, Array_double *v,
                                  double tolerance, size_t max_iterations);
extern Matrix_double *leslie_matrix(Array_double *age_class_surivor_ratio,
                                    Array_double *age_class_offspring);
#endif // LIZFCM_H
