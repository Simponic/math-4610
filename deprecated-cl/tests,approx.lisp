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

(test central-derivative-at
      :description "derivative at is within bounds"
      (let ((f (lambda (x) (* x x)))
            (x 2)
            (accepted-delta 0.02)
            (f-prime-at-x 4)
            (delta 0.01))
        (is (within-range-p
              (central-derivative-at f x delta)
              f-prime-at-x
              accepted-delta))))

(test fwd-derivative-at
      :description "forward derivative at is within bounds"
      (let ((f (lambda (x) (* x x)))
            (x 2)
            (accepted-delta 0.02)
            (f-prime-at-x 4)
            (delta 0.01))
        (is (within-range-p
              (forward-derivative-at f x delta)
              f-prime-at-x
              accepted-delta))))

(test bwd-derivative-at
      :description "backward derivative at is within bounds"
      (let ((f (lambda (x) (* x x)))
            (x 2)
            (accepted-delta 0.02)
            (f-prime-at-x 4)
            (delta 0.01))
        (is (within-range-p
              (backward-derivative-at f x delta)
              f-prime-at-x
              accepted-delta))))
