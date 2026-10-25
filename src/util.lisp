;;;; util.lisp

(in-package #:lindalog)

(defun multiset-equal-p (xs ys &key (test #'eql))
  "Check if the multisets XS and YS contain exactly the same elements in
the same quantity."
  (and (= (length xs) (length ys))
       (loop :with remaining := (copy-list ys)
             :for x :in xs
             :for match := (find x remaining :test test)
             :when match
               :do (setf remaining
                         (delete match remaining :count 1 :test #'eq))
             :finally (return (not remaining)))))
