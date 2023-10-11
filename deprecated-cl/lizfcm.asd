(asdf:defsystem "lizfcm"
                :version "0.1.0"
                :author "Elizabeth Hunt"
                :license "MIT"
                :components
                ((:file "utils,within-range" :depends-on ("utils,package"))
                 (:file "utils,table" :depends-on ("utils,package"))
                 (:file "utils,package")
                 (:file "approx,maceps" :depends-on ("approx,package"))
                 (:file "approx,derivative" :depends-on ("approx,package"))
                 (:file "approx,package")
                 (:file "vector,least-squares" :depends-on ("vector,package"))
                 (:file "vector,distance" :depends-on ("vector,norm" "vector,package"))
                 (:file "vector,norm" :depends-on ("vector,package"))
                 (:file "vector,package")))


(asdf:defsystem "lizfcm/tests"
                :author "Elizabeth Hunt"
                :license "MIT"
                :depends-on
                (:fiveam
                  :lizfcm)
                :components
                ((:file "tests,table" :depends-on ("tests,suite"))
                 (:file "tests,maceps" :depends-on ("tests,suite"))
                 (:file "tests,approx" :depends-on ("tests,suite"))
                 (:file "tests,vector" :depends-on ("tests,suite"))
                 (:file "tests,suite"))
                :perform
                (asdf:test-op (o c) (uiop:symbol-call
                                      :fiveam :run!
                                      (uiop:find-symbol* :lizfcm-test-suite :lizfcm/tests))))
