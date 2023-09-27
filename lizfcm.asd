(asdf:defsystem "lizfcm"
                :version "0.1.0"
                :author "Elizabeth Hunt"
                :license "MIT"
                :components ((:module "src"
                                      :components
                                      ((:module "utils"
                                                :components
                                                ((:file "within-range" :depends-on ("package"))
                                                 (:file "table" :depends-on ("package"))
                                                 (:file "package")))
                                       (:module "approx"
                                                :components
                                                ((:file "maceps" :depends-on ("package"))
                                                 (:file "derivative" :depends-on ("package"))
                                                 (:file "package")))
                                       (:module "vector"
                                                :components
                                                ((:file "distance" :depends-on ("norm" "package"))
                                                 (:file "norm" :depends-on ("package"))
                                                 (:file "package")))))))


(asdf:defsystem "lizfcm/tests"
                :author "Elizabeth Hunt"
                :license "MIT"
                :depends-on (:fiveam
                              :lizfcm)
                :components ((:module "tests"
                                      :components
                                      ((:file "table" :depends-on ("suite"))
                                       (:file "maceps" :depends-on ("suite"))
                                       (:file "approx" :depends-on ("suite"))
                                       (:file "vector" :depends-on ("suite"))
                                       (:file "suite"))))
                :perform (asdf:test-op (o c) (uiop:symbol-call
                                               :fiveam :run!
                                               (uiop:find-symbol* :lizfcm-test-suite :lizfcm/tests))))

