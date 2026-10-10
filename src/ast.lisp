;;;; ast.lisp

(in-package #:lindalog)

(defstruct ast-node
  (cst nil :type (or null cst:cst)
           :read-only t))

(defstruct (constant
            (:include ast-node)
            (:constructor make-constant (value &key cst)))
  (value nil :type (or number symbol)
             :read-only t))

(defstruct (variable
            (:include ast-node)
            (:constructor make-variable (name &key cst)))
  (name nil :type symbol
            :read-only t))

(defstruct (atom
            (:include ast-node)
            (:constructor make-atom (predicate args &key cst)))
  (predicate nil :type symbol
                 :read-only t)
  (args nil :type list
            :read-only t))

(defun atom-variables (atom)
  (remove-if-not #'variable-p
                 (atom-args atom)))

(defstruct (premise
            (:include ast-node)
            (:constructor make-premise (atom rd-p &key cst)))
  (atom nil :type atom
            :read-only t)
  (rd-p nil :type boolean
            :read-only t))

(defstruct (pred-decl
            (:include ast-node)
            (:constructor make-pred-decl (name store args &key cst)))
  (name nil :type symbol
            :read-only t)
  (store nil :type symbol
             :read-only t)
  (args nil :type list
            :read-only t))

(defstruct (init-fact
            (:include ast-node)
            (:constructor make-init-fact (atom &key cst)))
  (atom nil :type atom
            :read-only t))

(defstruct (rule
            (:include ast-node)
            (:constructor make-rule (name lhs rhs &key docstring cst)))
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
