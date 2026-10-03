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

   #:make-pred-decl
   #:pred-decl-p
   #:pred-decl-name
   #:pred-decl-store
   #:pred-decl-args

   #:make-init-fact
   #:init-fact-p
   #:init-fact-atom

   #:make-rule
   #:rule-p
   #:rule-name
   #:rule-docstring
   #:rule-lhs
   #:rule-rhs

   #:ast-equal-p

   #:make-database
   #:add-fact

   #:lindalog-error
   #:source-error
   #:syntax-error
   #:validation-error

   #:lindalog-error
   #:source-error
   #:syntax-error
   #:validation-error

   #:parse-term
   #:parse-atom
   #:parse-premise
   #:parse-conclusion
   #:parse-pred-decl
   #:parse-init-fact
   #:parse-rule
   ))
