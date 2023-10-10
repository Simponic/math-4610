(defpackage lizfcm/tests.maceps
  (:use :cl
        :fiveam
        :lizfcm.approx
        :lizfcm.utils
        :lizfcm/tests)
  (:export :approx-suite))
(in-package :lizfcm/tests.maceps)

(def-suite maceps-suite
           :in lizfcm-test-suite)
(in-suite maceps-suite)

(test maceps
      :description "double precision provides precision about (mac eps of single precision) squared"
      (let* ((maceps-computation-double (compute-maceps (lambda (x) x)
                                                        1.0d0
                                                        1.0d0))
             (maceps-computation-single (compute-maceps (lambda (x) x)
                                                        1.0
                                                        1.0))
             (last-double-h (cadar (last maceps-computation-double)))
             (last-single-h (cadar (last maceps-computation-single))))
        (is (within-range-p
              (- last-double-h (* last-single-h last-single-h))
              0
              last-single-h))))
