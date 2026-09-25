;;;; state.lisp

(in-package #:lindalog)

(defstruct (fact
            (:constructor make-fact (id atom)))
  (id 0 :type integer
        :read-only t)
  (atom nil :type atom
            :read-only t))

(defun fact-equal-p (a b)
  "Check if two facts are equal."
  (and (= (fact-id a) (fact-id b))
       (ast-equal-p (fact-atom a) (fact-atom b))))

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

(defun add-fact (fact database sc)
  "Add FACT to the SC table of DATABASE."
  (pushnew (ecase sc
             (:rd (database-rd-store database))
             (:in (database-in-store database))
             (:sub (database-sub-store database)))
           fact
           :test #'fact-equal-p))

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
