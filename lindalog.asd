;;;; lindalog.asd

(asdf:defsystem #:lindalog
  :description "A multiset rewriting language"
  :author "Kartik Singh <kartiksingh_ma@yahoo.com>"
  :version "0.0.1"
  :depends-on ("alexandria"
               "concrete-syntax-tree"
               "eclector-concrete-syntax-tree"
               )
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "ast")
     (:file "conditions")
     (:file "reader")
     (:file "parser")
     (:file "state")
     (:file "match")
     (:file "lindalog"))))
  :in-order-to ((asdf:test-op (asdf:test-op "lindalog/tests"))))

(asdf:defsystem #:lindalog/tests
  :description "Tests for Lindalog"
  :author "Kartik Singh <kartiksingh_ma@yahoo.com>"
  :depends-on ("fiveam"
               "lindalog"
               )
  :serial t
  :components
  ((:module "test"
    :components
    ((:file "package")
     (:file "ast")
     (:file "match")
     (:file "parser"))))
  :perform (asdf:test-op (op c)
                         (declare (ignore op c))
                         (unless (uiop:symbol-call :fiveam '#:run-all-tests)
                           (error "Lindalog test suite failed"))))
