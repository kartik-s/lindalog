;;;; conditions.lisp

(in-package #:lindalog)

(define-condition lindalog-error (error)
  ())

(define-condition source-error (lindalog-error)
  ((cst :initarg :cst
         :initform nil
         :reader source-error-cst)
   (message :initarg :message
            :reader source-error-message))
  (:report (lambda (condition stream)
             (let* ((cst (source-error-cst condition))
                    (range (and cst (cst:source cst))))
               (format stream "[~d-~d]: ~a"
                       (car range)
                       (cdr range)
                       (source-error-message condition))))))

(define-condition syntax-error (source-error)
  ())

(define-condition validation-error (source-error)
  ())
