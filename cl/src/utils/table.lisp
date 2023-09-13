(in-package :lizfcm.utils)

(defmacro table ((&key headers domain-order domain-values) &body body)
  `(cons
     ,headers
     (mapcar (lambda (tuple)
               (destructuring-bind ,domain-order tuple
                 (append tuple
                         (list
                           ,@body))))
             ,domain-values)))
