(in-package :lizfcm.utils)

(defun within-range-p (x true-value delta)
  (and (< x (+ true-value delta))
       (> x (- true-value delta))))
