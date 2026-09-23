;;;; lindalog.asd

(asdf:defsystem #:lindalog
  :description "Describe lindalog here"
  :author "Kartik Singh <kartiksingh_ma@yahoo.com>"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "ast")
               (:file "match")
               (:file "lindalog")))
