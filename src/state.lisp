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
  (facts-by-pred (make-hash-table :test #'equal)
   :type hash-table
   :read-only t)
  (next-id 0
   :type integer))

(defun all-fact-atoms (database)
  "Return a multiset of all the fact atoms in DATABASE."
  (loop :with table := (database-facts-by-pred database)
        :for k :being :the :hash-keys :of table
        :append (mapcar #'fact-atom (gethash k table))))

(defun add-fact (atom database env)
  "Add ATOM to DATABASE, selecting the correct storage class based on
ENV."
  (let* ((pred (atom-predicate atom))
         (pred-decl (lookup-pred-decl pred env))
         (store (pred-decl-store pred-decl))
         (fact (make-fact (incf (database-next-id database))
                          atom)))
    (if (eq :in store)
        (push fact (gethash (pred-ref-name pred)
                            (database-facts-by-pred database)))
        (pushnew fact (gethash (pred-ref-name pred)
                               (database-facts-by-pred database))
                 :key #'fact-atom
                 :test #'ast-equal-p))))

(defstruct (executable
            (:constructor %make-executable))
  (env nil
   :type environment
   :read-only t)
  (program nil
   :type program
   :read-only t))

(defun make-executable (program)
  (multiple-value-bind (program env)
      (validate-program program)
    (%make-executable :env env
                      :program program)))

(defstruct (image
            (:constructor %make-image))
  (executable nil
   :type executable
   :read-only t)
  (database (make-database)
   :type database
   :read-only t)
  (permanent-refractions (make-hash-table :test #'equal)
   :type hash-table
   :read-only t)
  (cycle-refractions (make-hash-table :test #'equal)
   :type hash-table
   :read-only t))

(defun make-image (program)
  (let* ((exe (make-executable program))
         (image (%make-image :executable exe)))
    (dolist (fact (program-init-facts program))
      (add-fact (init-fact-atom fact)
                (image-database image)
                (executable-env exe)))
    image))
