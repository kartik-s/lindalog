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

   #:make-premise
   #:premise-p
   #:premise-atom
   #:premise-rd-p

   #:make-rule
   #:rule-p
   #:rule-lhs
   #:rule-rhs

   #:ast-equal-p

   #:make-database
   #:add-fact
   ))
