;;;; lindalog.asd

(asdf:defsystem #:lindalog
  :description "A multiset rewriting language"
  :author "Kartik Singh <kartiksingh_ma@yahoo.com>"
  :version "0.0.1"
  :depends-on ("alexandria"
               )
  :serial t
  :components
  ((:module "src"
    :components
    ((:file "package")
     (:file "ast")
     (:file "conditions")
     (:file "parser")
     (:file "state")
     (:file "match")
     (:file "lindalog")))))

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
     (:file "parser")))))
