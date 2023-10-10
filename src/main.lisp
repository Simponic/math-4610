(load "lizfcm.asd")
(ql:quickload :lizfcm)

;; this is a collection showcasing the library developed for math4610, required
;; from the Shared Library definition

(defun smaceps ()
  (cadar (last (lizfcm.approx:compute-maceps
                 (lambda (x) x) 1.0 1.0))))

(defun dmaceps ()
  (cadar (last (lizfcm.approx:compute-maceps
                 (lambda (x) x) 1.0d0 1.0d0))))

(defun l2-norm (v)
  (let ((2-norm (lizfcm.vector:p-norm 2)))
    (funcall 2-norm v)))

(defun l1-norm (v)
  (let ((1-norm (lizfcm.vector:p-norm 1)))
    (funcall 1-norm v)))

(defun linf-norm (v)
  (lizfcm.vector:max-norm v))

(defun l2-distance (v1 v2)
  (let ((2-norm (lizfcm.vector:p-norm 2)))
    (lizfcm.vector:distance v1 v2 2-norm)))

(defun l1-distance (v1 v2)
  (let ((1-norm (lizfcm.vector:p-norm 1)))
    (lizfcm.vector:distance v1 v2 1-norm)))

(defun linf-distance (v1 v2)
  (lizfcm.vector:distance v1 v2 'lizfcm.vector:max-norm))

(defun f (x)
  (/ (- x 1) (+ x 1)))

(defun fprime (x)
  (/ 2 (expt (+ x 1) 2)))

(defmacro showcase (s-expr)
  `(format t "~a = ~a~%" ,(format nil "~a" s-expr) ,s-expr))

(defun main ()
  (showcase (smaceps))
  (showcase (dmaceps))
  (showcase (l2-norm '(1 2)))
  (showcase (l1-norm '(1 2)))
  (showcase (linf-norm '(1 2)))
  (showcase (l1-distance '(1 2) '(-3 4)))
  (showcase (l2-distance '(1 2) '(-3 4)))
  (showcase (linf-distance '(1 2) '(-3 4)))
  (showcase (lizfcm.vector:least-squares-reg '(1 2 3 4 5 6 7)
                                             '(0.5 3 2 3.5 5 6 7.5)))
  (showcase (lizfcm.approx:forward-derivative-at 'f 1 0.00001))
  (showcase (lizfcm.approx:central-derivative-at 'f 1 0.00001))
  (showcase (lizfcm.approx:backward-derivative-at 'f 1 0.00001)))

