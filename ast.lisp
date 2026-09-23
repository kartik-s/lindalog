;;;; ast.lisp

(in-package #:lindalog)

(defstruct constant
  (value nil :type (or number symbol)
             :read-only t))

(defstruct variable
  (name nil :type symbol
            :read-only t))

(defstruct atom
  (predicate nil :type symbol
                 :read-only t)
  (args nil :type list
            :read-only t))


(defstruct rule
  (lhs nil :type list
           :read-only t)
  (rhs nil :type list
           :read-only t))
