;; Copyright 2010-2018 Ben Lambert

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

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
