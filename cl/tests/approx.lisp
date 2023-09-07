(defpackage lizfcm/tests.approx
  (:use :cl
        :fiveam
        :lizfcm.approx
        :lizfcm.utils
        :lizfcm/tests)
  (:export :approx-suite))
(in-package :lizfcm/tests.approx)

(def-suite approx-suite
           :in lizfcm-test-suite)
(in-suite approx-suite)

(test derivative-at
      :description "derivative at is within bounds"
      (let ((f (lambda (x) (* x x)))
            (x 2)
            (f-prime-at-x 4)
            (delta 0.01))
        (is (within-range-p
              (derivative-at f x delta)
              f-prime-at-x
              0.1))))
