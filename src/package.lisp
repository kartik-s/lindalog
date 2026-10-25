;;;; package.lisp

(defpackage #:lindalog
  (:use #:cl)
  (:local-nicknames (#:cst #:concrete-syntax-tree))
  (:shadow #:variable #:atom #:unbound-variable)
  (:export

   #:multiset-equal-p

   #:ast-node-cst

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

   #:fact-id
   #:fact-atom

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
   #:semantic-error-related

   #:undeclared-predicate
   #:arity-mismatch
   #:variable-in-fact
   #:redundant-rd
   #:unbound-variable
   #:duplicate-predicate
   #:duplicate-rule

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

   #:validate-program

   #:make-database
   #:database-facts-by-pred
   #:database-next-id
   #:all-fact-atoms
   #:add-fact

   #:make-executable
   #:executable-env
   #:executable-program

   #:make-image
   #:image-executable
   #:image-database
   #:image-already-fired

   #:make-activation
   #:activation-refract-p
   #:activation-key
   #:activation-duration

   #:fire-rule
   ))
