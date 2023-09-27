(in-package :lizfcm.vector)

(defun p-norm (p)
  (lambda (v)
    (expt
      (reduce (lambda (acc x)
                (+ acc x))
              (mapcar (lambda (x)
                        (abs
                          (expt x p)))
                      v))
      (/ 1 p))))

(defun max-norm (v)
  (reduce (lambda (acc x)
            (max acc x))
          v))
