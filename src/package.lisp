;;;; package.lisp

(defpackage #:lindalog
  (:use #:cl)
  (:shadow #:variable #:atom)
  (:export

   #:make-constant
   #:constant-p
   #:constant-value

   #:make-variable
   #:variable-p
   #:variable-name

   #:make-atom
   #:atom-p
   #:atom-predicate
   #:atom-args

   #:make-rule
   #:rule-p
   #:rule-lhs
   #:rule-rhs
   ))
