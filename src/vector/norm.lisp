(in-package :lizfcm.vector)

(defun p-norm (p)
  (lambda (v)
    (expt
      (reduce #'+
              (mapcar (lambda (x)
                        (abs
                          (expt x p)))
                      v))
      (/ 1 p))))

(defun max-norm (v)
  (reduce #'max v))
