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

   #:make-pred-ref
   #:pred-ref-name

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

   #:make-program
   #:program-p
   #:program-predicates
   #:program-facts
   #:program-rules

   #:ast-equal-p

   #:make-database
   #:add-fact

   #:lindalog-error

   #:source-error
   #:source-error-message
   #:source-error-range

   #:read-error
   #:read-error-range

   #:syntax-error
   #:syntax-error-cst

   #:semantic-error
   #:semantic-error-ast

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
