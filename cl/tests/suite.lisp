(in-package :cl-user)
(defpackage lizfcm/tests
  (:use :cl
        :fiveam)
  (:export :run!
           :lizfcm-test-suite))
(in-package :lizfcm/tests)

(def-suite lizfcm-test-suite
    :description "The ultimate parent test suite")
