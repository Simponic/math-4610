(in-package :lizfcm.approx)

(defun compute-maceps (val f)
  (let* ((h val)
         (a val)
         (err val))
    (loop while (> err 0)
          do
          (progn
            (setf h (/ h 2)
                  err (abs (- (funcall f (+ a h))
                              (funcall f a)))))
          collect (list a h err))))
