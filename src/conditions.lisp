;;;; conditions.lisp

(in-package #:lindalog)

(define-condition lindalog-error (error)
  ())

(define-condition source-error (lindalog-error)
  ((message :initarg :message
            :reader source-error-message))
  (:report (lambda (condition stream)
             (let ((range (source-error-range condition)))
               (when range
                 (format stream "[~d-~d]: "
                         (car range)
                         (cdr range)))
               (write-string (source-error-message condition)
                             stream)))))

(defgeneric source-error-range (condition)
  (:documentation "The (start . end) source range CONDITION refers to, or NIL."))

(define-condition read-error (source-error)
  ((range :initarg :range
          :initform nil
          :reader read-error-range)))

(defmethod source-error-range ((condition read-error))
  (read-error-range condition))

(define-condition syntax-error (source-error)
  ((cst :initarg :cst
        :initform (error "SYNTAX-ERROR requires a CST")
        :reader syntax-error-cst)))

(defmethod source-error-range ((condition syntax-error))
  (let ((cst (syntax-error-cst condition)))
    (when cst (cst:source cst))))

(define-condition semantic-error (source-error)
  ((ast :initarg :ast
        :initform (error "SEMANTIC-ERROR requires an AST")
        :reader semantic-error-ast)
   (related :initarg :related
            :initform nil
            :reader semantic-error-related
            :documentation "A list of (NODE . NOTE) pairs for other relevant locations.")))

(defmethod source-error-range ((condition semantic-error))
  (let ((cst (ast-node-cst (semantic-error-ast condition))))
    (when cst (cst:source cst))))

(define-condition undeclared-predicate (semantic-error)
  ())

(define-condition arity-mismatch (semantic-error)
  ())

(define-condition variable-in-fact (semantic-error)
  ())

(define-condition redundant-rd (semantic-error)
  ())

(define-condition unbound-variable (semantic-error)
  ())

(define-condition duplicate-predicate (semantic-error)
  ())

(define-condition duplicate-rule (semantic-error)
  ())

(define-condition invalid-program (lindalog-error)
  ((errors :initarg :errors
           :initform nil
           :reader invalid-program-errors))
  (:report (lambda (condition stream)
             (format stream "~{~a~^~%~}"
                     (invalid-program-errors condition)))))

(define-condition program-syntax-errors (invalid-program)
  ())
(define-condition program-semantic-errors (invalid-program)
  ())
