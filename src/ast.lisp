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

(defstruct (pred-ref
            (:include ast-node)
            (:constructor make-pred-ref (name &key cst)))
  (name nil :type symbol :read-only t))

(defstruct (atom
            (:include ast-node)
            (:constructor make-atom (predicate args &key cst)))
  (predicate nil :type pred-ref
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
            (:constructor make-pred-decl (name args store &key docstring cst)))
  (name nil :type symbol
            :read-only t)
  (args nil :type list
            :read-only t)
  (store nil :type symbol
             :read-only t)
  (docstring nil :type (or null string)
                 :read-only t))

(defstruct (init-fact
            (:include ast-node)
            (:constructor make-init-fact (atom &key docstring cst)))
  (atom nil :type atom
            :read-only t)
  (docstring nil :type (or null string)
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

(defstruct (program
            (:include ast-node)
            (:constructor make-program (pred-decls init-facts rules)))
  (pred-decls nil :type list
                  :read-only t)
  (init-facts nil :type list
                  :read-only t)
  (rules nil :type list
             :read-only t))

(defgeneric ast-equal-p (a b)
  (:documentation "Check if two AST nodes are equivalent.")
  (:method (a b)
    nil))

(defmethod ast-equal-p ((a constant) (b constant))
  (let ((x (constant-value a))
        (y (constant-value b)))
    (etypecase x
      (symbol (and (symbolp y) (string= x y)))
      (number (and (numberp y) (= x y))))))

(defmethod ast-equal-p ((a variable) (b variable))
  (string= (variable-name a) (variable-name b)))

(defmethod ast-equal-p ((a pred-ref) (b pred-ref))
  (string= (pred-ref-name a) (pred-ref-name b)))

(defmethod ast-equal-p ((a atom) (b atom))
  (and (ast-equal-p (atom-predicate a) (atom-predicate b))
       (= (length (atom-args a))
          (length (atom-args b)))
       (every #'ast-equal-p
              (atom-args a)
              (atom-args b))))

(defmethod ast-equal-p ((a premise) (b premise))
  (and (ast-equal-p (premise-atom a)
                    (premise-atom b))
       (eq (premise-rd-p a)
           (premise-rd-p b))))

(defmethod ast-equal-p ((a pred-decl) (b pred-decl))
  (and (string= (pred-decl-name a) (pred-decl-name b))
       (eq (pred-decl-store a) (pred-decl-store b))
       (= (length (pred-decl-args a))
          (length (pred-decl-args b)))
       (every #'string=
              (pred-decl-args a)
              (pred-decl-args b))))

(defmethod ast-equal-p ((a init-fact) (b init-fact))
  (ast-equal-p (init-fact-atom a)
               (init-fact-atom b)))

(defun multiset-equal-p (xs ys &key (test #'eql))
  "Check if the multisets XS and YS contain exactly the same elements in
the same quantity."
  (and (= (length xs) (length ys))
       (loop :with remaining := (copy-list ys)
             :for x :in xs
             :for match := (find x remaining :test test)
             :when match
               :do (setf remaining
                         (delete match remaining :count 1 :test #'eq))
             :finally (return (not remaining)))))

(defmethod ast-equal-p ((a rule) (b rule))
  (and (string= (rule-name a)
                (rule-name b))
       (multiset-equal-p (rule-lhs a)
                         (rule-lhs b)
                         :test #'ast-equal-p)
       (multiset-equal-p (rule-rhs a)
                         (rule-rhs b)
                         :test #'ast-equal-p)))

(defmethod ast-equal-p ((a program) (b program))
  (and (and (= (length (program-pred-decls a))
               (length (program-pred-decls b)))
            (every #'ast-equal-p
                   (program-pred-decls a)
                   (program-pred-decls b)))
       (and (= (length (program-init-facts a))
               (length (program-init-facts b)))
            (every #'ast-equal-p
                   (program-init-facts a)
                   (program-init-facts b)))
       (and (= (length (program-rules a))
               (length (program-rules b)))
            (every #'ast-equal-p
                   (program-rules a)
                   (program-rules b)))))
