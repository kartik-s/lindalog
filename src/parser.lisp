;;;; parser.lisp

(in-package #:lindalog)

(defun variable-symbol-p (x)
  (and (symbolp x)
       (not (keywordp x))
       (<= 2 (length (symbol-name x)))
       (equal (char (symbol-name x) 0)
              #\?)))

(defun parse-term (term context)
  (cond ((variable-symbol-p term)
         (make-variable term))
        ((and (symbolp term)
              (not (string= '? term))
              (not (keywordp term)))
         (make-constant term))
        ((integerp term)
         (make-constant term))
        (t (error 'syntax-error
                  :form context
                  :message (format nil "invalid term: ~a"
                                   term)))))

(defun parse-atom (atom)
  "Parse a list into an atom AST node."
  (cond ((not (alexandria:proper-list-p atom))
         (error 'syntax-error
                :form atom
                :message (format nil "atom is not a proper list")))
        ((null atom)
         (error 'syntax-error
                :form atom
                :message (format nil "atom is an empty list")))
        (t
         (let ((predicate (first atom))
               (args (rest atom)))
           (cond ((not (symbolp predicate))
                  (error 'syntax-error
                         :form atom
                         :message (format nil "atom predicate is not a symbol: ~s"
                                          predicate)))
                 ((or (member predicate '(defrule defpredicate rd)
                              :test #'string=)
                      (eq :rd predicate))
                  (error 'syntax-error
                         :form atom
                         :message (format nil "reserved symbol used as an atom predicate: ~s"
                                          predicate)))
                 ((variable-symbol-p predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "variable used as an atom predicate: ~s"
                                          predicate)))
                 ((keywordp predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "keyword used as an atom predicate: ~s"
                                          predicate)))
                 ((null predicate)
                  (error 'syntax-error
                         :form atom
                         :message (format nil "NIL used as an atom predicate")))
                 (t (make-atom predicate (mapcar (lambda (arg)
                                                   (parse-term arg atom))
                                                 args))))))))

(defun parse-premise (premise)
  "Parse a list into a premise AST node."
  (cond ((not (alexandria:proper-list-p premise))
         (error 'syntax-error
                :form premise
                :message "premise must either be (:rd <atom>) or <atom>"))
        ((null premise)
         (error 'syntax-error
                :form premise
                :message "premise is NIL"))
        ((eq :rd (first premise))
         (if (/= 2 (length premise))
             (error 'syntax-error
                    :form premise
                    :message ":rd premise must be of the form (:rd <atom>)")
             (make-premise (parse-atom (second premise)) t)))
        ((and (symbolp (first premise))
              (string= 'rd (first premise)))
         (error 'syntax-error
                :form premise
                :message (format nil  "use :rd to mark a read-only-premise: ~s" premise)))
        (t (make-premise (parse-atom premise) nil))))
