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

(define-condition analysis-error (source-error)
  ((cst :initarg :cst
        :initform nil
        :reader analysis-error-cst)))

(defmethod source-error-range ((condition analysis-error))
  (let ((cst (analysis-error-cst condition)))
    (and cst (cst:source cst))))

(define-condition syntax-error (analysis-error)
  ())

(define-condition validation-error (analysis-error)
  ())

(define-condition invalid-program (lindalog-error)
  ((errors :initarg :errors
            :initform nil
            :reader invalid-program-errors)))

(define-condition program-syntax-errors (invalid-program) ())
(define-condition program-semantic-errors (invalid-program) ())
