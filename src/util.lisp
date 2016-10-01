;;;; Author Ben Lambert
;;;; ben@benjaminlambert.com

(in-package :gnuplot)

(defun all-equal (seq &key (test 'eql))
  "Check if *all* the elements in a sequence are equal to eachother."
  (let ((first (elt seq 0)))
    (every (lambda (x) (funcall test first x)) seq)))

(defun interval (a b)
  "Create a list of all integers from a to b."
  (loop for i from a to b collecting i))

(defun concat (&rest strings)
  "Shorthand function for concatenating strings..."
  (apply 'concatenate 'string strings))
