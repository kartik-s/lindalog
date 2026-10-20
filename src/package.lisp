;;;; package.lisp

(defpackage #:lindalog
  (:use #:cl)
  (:local-nicknames (#:cst #:concrete-syntax-tree))
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
   #:pred-decl-args
   #:pred-decl-store
   #:pred-decl-docstring

   #:make-init-fact
   #:init-fact-p
   #:init-fact-atom
   #:init-fact-docstring

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
   #:source-error-message
   #:source-error-range

   #:read-error
   #:read-error-range

   #:analysis-error
   #:analysis-error-cst

   #:syntax-error
   #:validation-error

   #:invalid-program
   #:invalid-program-errors

   #:program-syntax-errors
   #:program-semantic-errors

   #:parse-term
   #:parse-atom
   #:parse-premise
   #:parse-conclusion
   #:parse-pred-decl
   #:parse-init-fact
   #:parse-rule
   #:parse-toplevel
   #:parse-program

   #:read-program
   ))
