;;;; match.lisp

(in-package #:lindalog)

(defconstant +match-fail+ :match-fail
  "Indicates match failure")

(defconstant +no-bindings+ '((t . t))
  "Indicates match success with no variables")

(defun get-binding (var bindings)
  "Find a (var . val) pair in a binding list."
  (assoc var bindings :test #'ast-equal-p))

(defun binding-var (binding)
  "Get the variable part of a single binding"
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding"
  (cdr binding))

(defun lookup (var bindings)
  "Get the value associated with VAR in BINDINGS."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . val) pair to a binding list."
  (cons (cons var val)
        (if (eql +no-bindings+ bindings)
            nil
            bindings)))

(defun bindings-equal-p (a b)
  "Check if the two bindings are the same."
  (and (= (length a) (length b))
       (every (lambda (binding)
                (ast-equal-p (binding-val binding)
                             (lookup (binding-var binding) b)))
              a)))

(defun match-term (pattern fact bindings)
  "Match the term PATTERN against the ground term FACT, extending
BINDINGS as necessary. Return the resulting bindings on success, or
+MATCH-FAIL+ on failure."
  (cond ((and (variable-p pattern)
              (get-binding pattern bindings))
         (if (ast-equal-p fact (lookup pattern bindings))
             bindings
             +match-fail+))
        ((variable-p pattern)
         (extend-bindings pattern fact bindings))
        ((ast-equal-p pattern fact) bindings)
        (t +match-fail+)))

(defun match-atom (pattern fact bindings)
  "Match the atom PATTERN against the ground atom FACT, extending
BINDINGS as necessary. Return the resulting bindings on success,
or +MATCH-FAIL+ on failure."
  (declare (type atom pattern)
           (type atom fact))
  (labels ((match-args (pattern-args fact-args bindings)
             (cond ((and (null pattern-args)
                         (null fact-args))
                    bindings)
                   ((or (null pattern-args)
                        (null fact-args))
                    +match-fail+)
                   (t
                    (let ((new-bindings (match-term (first pattern-args)
                                                    (first fact-args)
                                                    bindings)))
                      (if (eq +match-fail+ new-bindings)
                          +match-fail+
                          (match-args (rest pattern-args)
                                      (rest fact-args)
                                      new-bindings)))))))
    (if (eq (atom-predicate pattern)
            (atom-predicate fact))
        (match-args (atom-args pattern)
                    (atom-args fact)
                    bindings)
        +match-fail+)))

(defstruct match
  (bindings +no-bindings+
   :type list
   :read-only t)
  (read-facts nil
   :type list
   :read-only t)
  (consumed-facts nil
   :type list
   :read-only t))

(defun extend-match-fact (match fact consume-p)
  (let ((read-facts (match-read-facts match))
        (consumed-facts (match-consumed-facts match)))
    (make-match
     :bindings (match-bindings match)
     :read-facts (if consume-p
                     read-facts
                     (cons fact read-facts))
     :consumed-facts (if consume-p
                         (cons fact consumed-facts)
                         consumed-facts))))

(defun extend-match-bindings (match bindings)
  (make-match
   :bindings bindings
   :read-facts (match-read-facts match)
   :consumed-facts (match-consumed-facts match)))

(defun match-premises (premises database match &optional (accept-p (constantly t)))
  "Find a match in DATABASE for the conjunction of PREMISES that
satisfies ACCEPT-P, or +MATCH-FAIL+ if none exists."
  (if (null premises)
      (if (funcall accept-p match)
          match
          +match-fail+)
      (let* ((premise (first premises))
             (other-premises (rest premises))
             (pred (atom-predicate (premise-atom premise)))
             (sc (gethash pred (database-predicate-scs database)))
             (table (ecase sc
                      (:rd (database-rd-store database))
                      (:in (database-in-store database))
                      (:sub (database-sub-store database)))))
        (loop :for fact :in (gethash pred table)
              :unless (and (eq :in sc)
                           (not (premise-rd-p premise))
                           (member (fact-id fact)
                                   (match-consumed-facts match)
                                   :key #'fact-id))
                :do (let ((first-bindings (match-atom (premise-atom premise)
                                                      (fact-atom fact)
                                                      (match-bindings match))))
                      (unless (eq +match-fail+ first-bindings)
                        (let ((new-match (match-premises other-premises
                                                         database
                                                         (extend-match-bindings
                                                          (extend-match-fact
                                                           match
                                                           fact
                                                           (and (eq :in sc)
                                                                (not (premise-rd-p premise))))
                                                          first-bindings)
                                                         accept-p)))
                          (unless (eq +match-fail+ new-match)
                            (return new-match)))))
              :finally (return +match-fail+)))))
