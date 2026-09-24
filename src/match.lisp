;;;; match.lisp

(in-package #:lindalog)

(defconstant +match-fail+ :match-fail
  "Indicates match failure")

(defconstant +no-bindings+ '((t . t))
  "Indicates match success with no variables")

(defun get-binding (var bindings)
  "Find a (var . val) pair in a binding list."
  (assoc var bindings))

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
