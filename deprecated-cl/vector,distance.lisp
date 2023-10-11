(in-package :lizfcm.vector)

(defun distance (v1 v2 norm)
  (let* ((d (mapcar #'- v1 v2))
         (length (funcall norm d)))
    length))
