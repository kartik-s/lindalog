;;;; state.lisp

(in-package #:lindalog)

(defstruct (fact
            (:constructor make-fact (id atom)))
  (id 0 :type integer
        :read-only t)
  (atom nil :type atom
            :read-only t))

(defstruct database
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
  (next-id 0
   :type integer))

(defstruct interpreter-state
  (rules nil
   :type list
   :read-only t)
  (database (make-database)
   :type database
   :read-only t)
  (already-fired (make-hash-table)
   :type hash-table
   :read-only t))
