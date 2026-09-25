;;;; interpreter.lisp

(in-package #:lindalog)

(defstruct interpreter-state
  (rules nil
   :type list
   :read-only t)
  (predicate-scs (make-hash-table)
   :type hash-table
   :read-only t)
  (rd-store (make-hash-table)
   :type hash-table
   :read-only t)
  (in-store (make-hash-table)
   :type hash-table
   :read-only t)
  (sub-store (make-hash-table)
   :type hash-table
   :read-only t)
  (already-fired (make-hash-table)
   :type hash-table
   :read-only t))
