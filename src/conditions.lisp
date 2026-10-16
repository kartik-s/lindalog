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
               (when range
                 (format stream "[~d-~d]: "
                         (car range)
                         (cdr range)))
               (write-string (source-error-message condition)
                             stream)))))

(define-condition syntax-error (source-error)
  ())

(define-condition validation-error (source-error)
  ())
