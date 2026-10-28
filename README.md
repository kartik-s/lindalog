# Lindalog
Lindalog is a synchronous multiset rewriting language. A program is a set of rules that consume or read the facts they match, then add new facts to a shared store. Programs react to inputs in discrete instants.

**This is a work in progress.** The reader, parser, validator and reference interpreter
are written in Common Lisp.

## Example
`examples/combustion.ldl` burns hydrogen, 2 H₂ + O₂ → 2 H₂O, when a
spark arrives:

```lisp
(defpred h2 () (:store :in))
(defpred o2 () (:store :in))
(defpred h2o () (:store :in))
(defpred spark () (:store :sig))

(deffact (h2))
(deffact (h2))
(deffact (h2))
(deffact (o2))
(deffact (o2))

(defrule combustion
  (:when (spark) (h2) (h2) (o2))
  (:then (h2o) (h2o)))
```

The rule fires once, consuming two `h2` and one `o2`. It can't fire
again, because only one `h2` is left, so the store ends with one `h2`,
one `o2` and two `h2o`.

## Running it
Requires SBCL and Quicklisp. Clone this repository into
`~/quicklisp/local-projects/`, then at the REPL:

```lisp
(ql:quickload :lindalog)

(defparameter *image*
  (with-open-file (s "examples/combustion.ldl")
    (lindalog:make-image
     (lindalog:parse-program (lindalog:read-program s)))))

;; React to one instant containing the signal (spark)
(lindalog:react *image*
                (list (lindalog:make-atom
                       (lindalog:make-pred-ref 'lindalog-user::spark) '())))

;; Print the store
(let ((*package* (find-package '#:lindalog-user))
      (*print-case* :downcase))
  (dolist (atom (lindalog:all-fact-atoms (lindalog:image-database *image*)))
    (format t "(~a~{ ~a~})~%"
            (lindalog:pred-ref-name (lindalog:atom-predicate atom))
            (mapcar #'lindalog:constant-value (lindalog:atom-args atom)))))
```

Output:

```
(h2)
(o2)
(h2o)
(h2o)
```

Run the tests with `(asdf:test-system :lindalog)`.
