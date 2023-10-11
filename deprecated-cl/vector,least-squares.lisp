(in-package :lizfcm.vector)

(defun least-squares-reg (x y)
  (let* ((n (length x))
         (sum-y (reduce #'+ y))
         (sum-x (reduce #'+ x))
         (sum-xy (reduce #'+ (mapcar #'* x y)))
         (sum-xsquared (reduce #'+ (mapcar #'* x x)))
         (b (/ (- (* sum-y sum-xsquared) (* sum-x sum-xy))
               (- (* n sum-xsquared) (* sum-x sum-x))))
         (a (/ (- (* n sum-xy) (* sum-x sum-y))
               (- (* n sum-xsquared) (* sum-x sum-x)))))
    (list a b)))

