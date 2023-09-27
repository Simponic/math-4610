(in-package :lizfcm.approx)

(defun compute-maceps (f a init)
  (let ((h init)
        (err init))
    (loop while (> err 0)
          do
          (setf h (/ h 2)
                err (abs (- (funcall f (+ a h))
                            (funcall f a))))
          collect (list a h err))))
