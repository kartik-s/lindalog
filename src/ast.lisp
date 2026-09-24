;;;; ast.lisp

(in-package #:lindalog)

(defstruct (constant
            (:constructor make-constant (value)))
  (value nil :type (or number symbol)
             :read-only t))

(defstruct (variable
            (:constructor make-variable (name)))
  (name nil :type symbol
            :read-only t))

(defstruct (atom
            (:constructor make-atom (predicate args)))
  (predicate nil :type symbol
                 :read-only t)
  (args nil :type list
            :read-only t))

(defstruct (rule
            (:constructor make-rule (lhs rhs)))
  (lhs nil :type list
           :read-only t)
  (rhs nil :type list
           :read-only t))
