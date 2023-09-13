(in-package :lizfcm.approx)

(defun derivative-at (f x &optional (delta 0.01))
  (let* ((x2 (+ x delta))
         (x1 (- x delta))
         (y2 (apply f (list x2)))
         (y1 (apply f (list x1))))
    (/ (- y2 y1)
       (- x2 x1))))
