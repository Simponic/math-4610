(defpackage lizfcm/tests.table
  (:use :cl
        :fiveam
        :lizfcm.utils
        :lizfcm/tests)
  (:export :approx-suite))
(in-package :lizfcm/tests.table)

(def-suite table-suite
           :in lizfcm-test-suite)
(in-suite table-suite)

(defun fib (n)
  (cond ((< n 2) n)
        (t (+ (fib (- n 1)) (fib (- n 2))))))

(test table-of-fib-vals
      :description "table generates correctly"
      (let* ((headers '("n" "fib(n)"))
             (n-values '((1) (2) (3) (4)))
             (expected `(("n" "fib(n)")
                         (1 ,(fib 1))
                         (2 ,(fib 2))
                         (3 ,(fib 3))
                         (4 ,(fib 4))))
             (tabled (lizfcm.utils:table (:headers headers
                                                   :domain-order (n)
                                                   :domain-values n-values)
                                         (fib n))))
        (is (equal expected tabled))))

