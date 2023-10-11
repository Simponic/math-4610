(in-package :cl-user)
(defpackage lizfcm.approx
  (:use :cl)
  (:export :central-derivative-at
           :forward-derivative-at
           :backward-derivative-at
           :compute-maceps))
