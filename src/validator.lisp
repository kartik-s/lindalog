;;;; validator.lisp

(in-package #:lindalog)

(defstruct environment
  (pred-decls (make-hash-table :test #'equal)
   :type hash-table
   :read-only t))

(defun lookup-pred-decl (pred-ref env)
  (gethash (symbol-name (pred-ref-name pred-ref))
           (environment-pred-decls env)))

(defgeneric validate (node env)
  (:documentation "Return a list of NODE's semantic errors according to ENV, or NIL if
there are none."))

(defmethod validate ((node constant) env)
  "All constant checks are syntactic, so always return NIL."
  nil)

(defmethod validate ((node variable) env)
  "All variable checks are syntactic, so always return NIL."
  nil)

(defmethod validate ((node pred-ref) env)
  (let ((pred-decl (lookup-pred-decl node env)))
    (unless pred-decl
      (list (make-condition 'undeclared-predicate
                            :ast node
                            :format-control "undeclared predicate ~a"
                            :format-arguments (list (pred-ref-name node)))))))

(defmethod validate ((node atom) env)
  (let ((pred-decl (lookup-pred-decl (atom-predicate node) env)))
    (append
     (validate (atom-predicate node) env)
     (when (and pred-decl
            (/= (length (pred-decl-args pred-decl))
                (length (atom-args node))))
       (list (make-condition 'arity-mismatch
                             :ast node
                             :format-control "~a takes ~d argument~:p, got ~d"
                             :format-arguments (list (pred-decl-name pred-decl)
                                                     (length (pred-decl-args pred-decl))
                                                     (length (atom-args node)))
                             :related (list (cons pred-decl "declared here"))))))))

(defmethod validate ((node pred-decl) env)
  "All predicate checks are syntactic, so always return NIL."
  nil)

(defmethod validate ((node init-fact) env)
  (let* ((atom (init-fact-atom node))
         (atom-errors (validate atom env)))
    (append
     atom-errors
     (loop :for arg :in (atom-args atom)
           :when (variable-p arg)
             :collect (make-condition 'variable-in-fact
                                      :ast arg
                                      :format-control "expected ground term, got variable ~a"
                                      :format-arguments (list (variable-name arg)))))))

(defmethod validate ((node premise) env)
  (let* ((atom (premise-atom node))
         (pred-decl (lookup-pred-decl (atom-predicate atom) env)))
    (append
     (validate atom env)
     (when (and pred-decl
                (premise-rd-p node)
                (not (eq :in (pred-decl-store pred-decl))))
       (list (make-condition 'redundant-rd
                             :ast node
                             :format-control "premise marked :rd, but predicate ~a is not :in"
                             :format-arguments (list (pred-decl-name pred-decl))
                             :related (list (cons pred-decl "declared here"))))))))

(defmethod validate ((node rule) env)
  (append
   (loop :for premise :in (rule-lhs node)
         :append (validate premise env))
   (loop :for conclusion :in (rule-rhs node)
         :append (validate conclusion env))
   (let* ((lhs-vars (collect-variables (rule-lhs node)))
          (unbound-vars (remove-if (lambda (var)
                                     (member var lhs-vars :test #'ast-equal-p))
                                   (collect-variables (rule-rhs node)))))
     (loop :for var :in unbound-vars
           :collect (make-condition 'unbound-variable
                                    :ast var
                                    :format-control "unbound variable ~a"
                                    :format-arguments (list (variable-name var)))))))

(defun validate-program (program)
  "Check PROGRAM for semantic errors, raising PROGRAM-SEMANTIC-ERRORS if
there are any, and returning PROGRAM if there are none."
  (multiple-value-bind (env duplicate-pred-errors)
      (loop :with pred-table := (make-hash-table :test #'equal)
            :for pred :in (program-pred-decls program)
            :for pred-name := (symbol-name (pred-decl-name pred))
            :if (gethash pred-name pred-table)
              :collect (make-condition 'duplicate-predicate
                                       :ast pred
                                       :format-control "duplicate predicate ~a"
                                       :format-arguments (list pred-name)
                                       :related (list (cons (gethash pred-name pred-table)
                                                            "declared here")))
                :into duplicate-errors
            :else
              :do (setf (gethash pred-name pred-table)
                        pred)
            :finally (return (values (make-environment :pred-decls pred-table)
                                     duplicate-errors)))
    (let ((duplicate-rule-errors
            (loop :with rule-set := (make-hash-table :test #'equal)
                  :for rule :in (program-rules program)
                  :for rule-name := (symbol-name (rule-name rule))
                  :for existing-rule := (gethash rule-name rule-set)
                  :if existing-rule
                    :collect (make-condition 'duplicate-rule
                                             :ast rule
                                             :format-control "duplicate rule ~a"
                                             :format-arguments (list (rule-name rule))
                                             :related (list (cons existing-rule "declared here")))
                  :else
                    :do (setf (gethash rule-name rule-set)
                              rule))))
      (flet ((collect-errors (nodes)
               (loop :for node :in nodes
                     :append (validate node env))))
        (let* ((preds-result (collect-errors (program-pred-decls program)))
               (facts-result (collect-errors (program-init-facts program)))
               (rules-result (collect-errors (program-rules program)))
               (errors (append duplicate-pred-errors
                               duplicate-rule-errors
                               preds-result
                               facts-result
                               rules-result)))
          (if errors
              (error 'program-semantic-errors :errors errors)
              program))))))
