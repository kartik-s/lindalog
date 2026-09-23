;;;; match.lisp

(in-package #:lindalog)

(defconstant +match-fail+ :match-fail
  "Constant indicating a match failed")

(defun match-atom (pattern fact bindings)
  "Match the atom PATTERN against the ground atom FACT, extending
BINDINGS as necessary. Return the resulting bindings on success,
or +MATCH-FAIL+ on failure."
  (declare (type atom pattern)
           (type atom fact)
           (type list bindings))
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
