;;;; validator.lisp

(in-package #:lindalog)

(defstruct environment
  (pred-decls (make-hash-table)
   :type hash-table
   :read-only t))

(defun lookup-pred-decl (pred-ref env)
  "Get the predicate declaration that PRED-REF refers to in ENV."
  (gethash (pred-ref-name pred-ref)
           (environment-pred-decls env)))

(defun premise-consuming-p (premise env)
  "Return T if PREMISE is consumed, otherwise NIL."
  (and (not (premise-rd-p premise))
       (eq :in (pred-decl-store
                (lookup-pred-decl
                 (atom-predicate (premise-atom premise))
                 env)))))

(defun rule-consuming-p (rule env)
  "Return T if RULE consumes any of its premises, otherwise NIL."
  (some (lambda (premise)
          (premise-consuming-p premise env))
        (rule-lhs rule)))

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
  (flet ((collect-duplicates (nodes condition-type)
           (loop :with table := (make-hash-table)
                 :for node :in nodes
                 :for name := (ast-node-name node)
                 :for existing := (gethash name table)
                 :if existing
                   :collect (make-condition condition-type
                                            :ast node
                                            :format-arguments (list name)
                                            :related (list (cons existing "declared here")))
                     :into errors
                 :else
                   :do (setf (gethash name table) node)
                 :finally (return (values errors table))))
         (collect-errors (nodes env)
           (loop :for node :in nodes
                 :append (validate node env))))
    (multiple-value-bind (duplicate-pred-errors pred-table)
        (collect-duplicates (program-pred-decls program)
                            'duplicate-predicate)
      (let* ((env (make-environment :pred-decls pred-table))
             (duplicate-rule-errors (collect-duplicates (program-rules program)
                                                        'duplicate-rule))
             (preds-result (collect-errors (program-pred-decls program) env))
             (facts-result (collect-errors (program-init-facts program) env))
             (rules-result (collect-errors (program-rules program) env))
             (errors (stable-sort (append duplicate-pred-errors
                                          duplicate-rule-errors
                                          preds-result
                                          facts-result
                                          rules-result)
                                  #'<
                                  :key (lambda (e)
                                         (or (car (source-error-range e)) 0)))))
        (if errors
            (error 'program-semantic-errors :errors errors)
            (values program env))))))
