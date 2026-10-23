;;;; reader.lisp

(in-package #:lindalog)

(defclass lindalog-client (eclector.concrete-syntax-tree:cst-client)
  ()
  (:documentation "Eclector client that rejects package syntax and circular input."))

(defmethod eclector.reader:interpret-symbol-token ((client lindalog-client) stream token marker-1 marker-2)
  "Reject package syntax."
  (declare (ignore marker-2))
  (case marker-1
    ((nil 0) (call-next-method))
    (otherwise
     (let ((end (file-position stream)))
       (error 'read-error
              :range (cons (- end (length token))
                           end)
              :format-control "package syntax not allowed: ~a"
              :format-arguments (list token))))))

(defmethod eclector.reader:note-labeled-object ((client lindalog-client) stream label parent)
  "Reject circular input."
  (declare (ignore parent))
  (let ((end (file-position stream)))
    (error 'lindalog:read-error
           :range (cons (- end (length (format nil "~d" label)))
                        end)
           :format-control "labels not allowed: #~d="
           :format-arguments (list label))))

(defpackage #:lindalog-user
  (:use)
  (:documentation "Symbols read from Lindalog programs are interned here."))

(defun read-program (stream)
  "Read a program from STREAM into a list of toplevel CSTs, signaling
READ-ERROR if the program is malformed."
  (let ((*package* (find-package '#:lindalog-user))
        (*read-eval* nil)
        (eclector.reader:*client* (make-instance 'lindalog-client)))
    (handler-case
        (loop :for cst := (eclector.concrete-syntax-tree:read stream nil stream)
              :until (eq stream cst)
              :collect cst)
      ((or eclector.base:stream-position-reader-error eclector.base:end-of-file) (e)
        (error 'read-error
               :range (let ((begin (+ (eclector.base:stream-position e)
                                      (eclector.base:position-offset e))))
                        (cons begin (+ begin (eclector.base:range-length e))))
               :format-control "~a"
               :format-arguments (list (princ-to-string e)))))))
