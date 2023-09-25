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
      (let* ((maceps-computation-double (compute-maceps 1.0d0
                                                        (lambda (x) x)))
             (maceps-computation-single (compute-maceps 1.0
                                                        (lambda (x) x)))
             (last-double-h (cadar (last maceps-computation-double)))
             (last-single-h (cadar (last maceps-computation-single))))
        (is (within-range-p
              (- last-double-h (* last-single-h last-single-h))
              0
              last-single-h))))
