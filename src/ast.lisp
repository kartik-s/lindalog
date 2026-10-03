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

(defun atom-variables (atom)
  (remove-if-not #'variable-p
                 (atom-args atom)))

(defstruct (premise
            (:constructor make-premise (atom rd-p)))
  (atom nil :type atom
            :read-only t)
  (rd-p nil :type boolean
            :read-only t))

(defstruct (pred-decl
            (:constructor make-pred-decl (name store args)))
  (name nil :type symbol
            :read-only t)
  (store nil :type symbol
             :read-only t)
  (args nil :type list
            :read-only t))

(defstruct (init-fact
            (:constructor make-init-fact (atom)))
  (atom nil :type atom
            :read-only t))

(defstruct (rule
            (:constructor make-rule (name lhs rhs &key docstring)))
  (name nil :type symbol
            :read-only t)
  (docstring nil :type (or null string)
                :read-only t)
  (lhs nil :type list
           :read-only t)
  (rhs nil :type list
           :read-only t))

(defun ast-equal-p (a b)
  (typecase a
    (constant
     (and (constant-p b)
          (if (symbolp (constant-value a))
              (string= (constant-value a)
                       (constant-value b))
              (= (constant-value a)
                 (constant-value b)))))
    (variable
     (and (variable-p b)
          (string= (variable-name a)
                   (variable-name b))))
    (atom
     (and (atom-p b)
          (string= (atom-predicate a)
                   (atom-predicate b))
          (= (length (atom-args a))
             (length (atom-args b)))
          (every #'ast-equal-p
                 (atom-args a)
                 (atom-args b))))
    (premise
     (and (ast-equal-p (premise-atom a)
                       (premise-atom b))
          (eq (premise-rd-p a)
              (premise-rd-p b))))
    (pred-decl
     (and (string= (pred-decl-name a)
                   (pred-decl-name b))
          (eq (pred-decl-store a)
              (pred-decl-store b))
          (= (length (pred-decl-args a))
             (length (pred-decl-args b)))
          (every #'string=
                 (pred-decl-args a)
                 (pred-decl-args b))))
    (init-fact
     (ast-equal-p (init-fact-atom a)
                  (init-fact-atom b)))
    (rule
     (and (rule-p b)
          (string= (rule-name a)
                   (rule-name b))
          (= (length (rule-lhs a))
             (length (rule-lhs b)))
          (= (length (rule-rhs a))
             (length (rule-rhs b)))
          (every #'ast-equal-p
                 (rule-lhs a)
                 (rule-lhs b))
          (every #'ast-equal-p
                 (rule-rhs a)
                 (rule-rhs b))))
    (otherwise nil)))
