(defpackage lizfcm/tests.vector
  (:use :cl
        :fiveam
        :lizfcm.vector
        :lizfcm.utils
        :lizfcm/tests)
  (:export :vector-suite))
(in-package :lizfcm/tests.vector)

(def-suite vector-suite
           :in lizfcm-test-suite)
(in-suite vector-suite)

(test p-norm
      :description "computes p-norm"
      (let ((v '(1 1))
            (length (sqrt 2))
            (2-norm (p-norm 2)))
        (is (within-range-p (funcall 2-norm v)
                            length
                            0.00001))))

(test vector-distance
      :description "computes distance via norm"
      (let ((v1 '(0 0))
            (v2 '(1 1))
            (dist (sqrt 2)))
        (is (within-range-p (distance v1 v2 (p-norm 2))
                            dist
                            0.00001))))

(test least-squares
      :description "least squares is correct enough"
      (let ((x '(0 1 2 3 4))
            (y '(1 2 3 4 5)))
        (destructuring-bind (m b) (lizfcm.vector:least-squares-reg x y)
          (is (within-range-p m 1 0.00001))
          (is (within-range-p b 1 0.00001))))
      (let ((x '(1 2 3 4 5 6 7))
            (y '(0.5 3 2 3.5 5 6 7.5)))
        (destructuring-bind (m b) (lizfcm.vector:least-squares-reg x y)
          (is (within-range-p m 1 0.3))))) ;; just a guestimate for best fit
