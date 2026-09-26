;;;; conditions.lisp

(in-package #:lindalog)

(define-condition lindalog-error (error)
  ())

(define-condition source-error (lindalog-error)
  ((form :initarg :form
         :initform nil
         :reader source-error-form)
   (location :initarg :location
             :initform nil
             :reader source-error-location)
   (message :initarg :message
            :reader source-error-message))
  (:report (lambda (condition stream)
             (format stream "~@[~a: ~]~a"
                     (source-error-location condition)
                     (source-error-message condition)))))

(define-condition syntax-error (source-error)
  ())

(define-condition validation-error (source-error)
  ())
