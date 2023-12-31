(in-package :lizfcm.approx)

(defun compute-maceps (f a init)
  (let ((h init)
        (err init))
    (loop collect (list a h err)
          do
          (setf h (/ h 2)
                err (abs (- (funcall f (+ a h))
                            (funcall f a))))
          while (> err 0))))

