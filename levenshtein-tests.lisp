;; Copyright 2020, Andrew C. Young <andrew@vaelen.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(defpackage :levenshtein-tests
  (:use :common-lisp)
  (:export :run))

(in-package :levenshtein-tests)

(defparameter *test-data*
  '(("cat" "hat" 1)
    ("cat" "catt" 1)
    ("cat" "ct" 1)
    ("cat" "c" 2)
    ("cat" "" 3)
    ("book" "back" 2)
    ("kitten" "sitting" 3)
    ("Saturday" "Sunday" 3)
    ("abcdefghijk" "abcdefghiijk" 1)
    ("abcdefghijk" "abcdefghjk" 1)
    ("abcdefghijk" "abcdefghIjk" 1)
    ("abcdefghijk" "abdefGhiijkl" 4)))

(defparameter *methods*
  (list #'levenshtein:simple
        #'levenshtein:iterate))

(defun test-distance (method string1 string2 expected)
  "Test the distance function using specific inputs and a specific method."
  (let ((result (levenshtein:distance string1 string2 :method method)))
    (if (not (= expected result))
        (format t
                "ERROR: Distance test failed. Method: ~A, Strings: ~A ~A, Expected: ~A, Got: ~A~%"
                method string1 string2 expected result))))

(defun run ()
  "Run distance tests for all methods."
  (loop for method in *methods* do
       (loop for data in *test-data* do
            (test-distance method (first data) (second data) (third data)))))

