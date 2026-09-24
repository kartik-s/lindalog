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

(defun ast-equal-p (a b)
  (typecase a
    (constant
     (and (constant-p b)
          (eql (constant-value a)
               (constant-value b))))
    (variable
     (and (variable-p b)
          (eq (variable-name a)
              (variable-name b))))
    (atom
     (and (atom-p b)
          (eq (atom-predicate a)
              (atom-predicate b))
          (every #'ast-equal-p
                 (atom-args a)
                 (atom-args b))))
    (rule
     (and (rule-p b)
          (every #'ast-equal-p
                 (rule-lhs a)
                 (rule-lhs b))
          (every #'ast-equal-p
                 (rule-rhs a)
                 (rule-rhs b))))
    (otherwise nil)))
