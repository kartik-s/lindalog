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
  (rd-store (make-hash-table)
   :type hash-table
   :read-only t)
  (in-store (make-hash-table)
   :type hash-table
   :read-only t)
  (sig-store (make-hash-table)
   :type hash-table
   :read-only t)
  (next-id 0
   :type integer))

(defun add-fact (atom database env)
  "Add ATOM to DATABASE, selecting the correct storage class based on
ENV."
  (let* ((pred (atom-predicate atom))
         (pred-decl (lookup-pred-decl pred env))
         (sc (pred-decl-store pred-decl))
         (store (ecase sc
                  (:rd (database-rd-store database))
                  (:in (database-in-store database))
                  (:sig (database-sig-store database))))
         (fact (make-fact (incf (database-next-id database))
                          atom)))
    (if (eq :in sc)
        (push fact (gethash (pred-ref-name pred) store))
        (pushnew fact (gethash (pred-ref-name pred) store)
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
  (already-fired (make-hash-table)
   :type hash-table
   :read-only t))

(defun boot (image)
  (let* ((exe (image-executable image))
         (program (executable-program exe)))
    (dolist (fact (program-init-facts program))
      (add-fact (init-fact-atom fact)
                (image-database image)
                (executable-env exe)))
    image))

(defun make-image (program)
  (let* ((exe (make-executable program))
         (image (%make-image :executable exe)))
    (boot image)))
