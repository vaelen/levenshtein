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

(defpackage :levenshtein
  (:use :common-lisp)
  (:export
   #:distance
   #:simple
   #:iterate
   #:test
   #:main))

(in-package :levenshtein)

(defun simple (s1 s2 &optional (limit -1) (dist 0))
  "Returns the levenshtein distance between two strings using recursion."
  (cond ((not (listp s1)) (simple (coerce s1 'list) s2 limit dist))  ; Coerce inputs to lists
        ((not (listp s2)) (simple s1 (coerce s2 'list) limit dist))  ; Coerce inputs to lists
        ((and (> limit 0) (> dist limit)) dist)                      ; If the limit has been passed, return
        ((= (length s1) 0) (+ (length s2) dist))                     ; When the shorter string ends
        ((= (length s2) 0) (+ (length s1) dist))                     ;     add the remaining character count
        ((equal (first s1) (first s2))
         (simple (rest s1) (rest s2) limit dist))                    ; Match
        (t (min
            (simple (rest s1) (rest s2) limit (1+ dist))             ; Substitution
            (simple s1 (rest s2) limit (1+ dist))                    ; Insertion
            (simple (rest s1) s2 limit (1+ dist))))))                ; Deletion

(defun iterate (string1 string2 &optional (limit -1) (dist 0))
  "Returns the levenshtein distance between two strings using iteration."
  (if (not (listp string1)) (setf string1 (coerce string1 'list)))   ; Coerce inputs to lists
  (if (not (listp string2)) (setf string2 (coerce string2 'list)))   ; Coerce inputs to lists
  (let ((queue (list (vector string1 string2 dist)))
        (result nil))
    (flet ((add-to-queue (s1 s2 d)
             (push (vector s1 s2 d) queue))
           (add-result (d)
             (setf result (if (not result) d (min result d)))))
      (loop while (not (null queue)) do
           (let* ((entry (pop queue))
                  (s1 (elt entry 0))
                  (s2 (elt entry 1))
                  (d (elt entry 2)))
             (cond ((and result (>= d result)))
                   ((and (> limit 0) (> d limit)) (add-result d))
                   ((= (length s1) 0) (add-result (+ d (length s2)))) ; End of first string
                   ((= (length s2) 0) (add-result (+ d (length s1)))) ; End of second string
                   ((equal (first s1) (first s2))                     ; Match
                    (add-to-queue (rest s1) (rest s2) d))
                   (t (progn
                       (add-to-queue (rest s1) (rest s2) (1+ d))      ; Substitution
                       (add-to-queue s1 (rest s2) (1+ d))             ; Insertion
                       (add-to-queue (rest s1) s2 (1+ d))))))         ; Deletion
         finally (return result)))))

(defun distance (string1 string2 &key (limit -1) (method #'iterate) (distance 0))
  "Returns the levenshtein distance between two strings."
  (funcall method string1 string2 limit distance))

(defun test (string1 string2)
  "Tests the execution time of all levenshtein implementations"
  (loop for method in (list #'simple #'iterate) do
       (progn
         (format 't "Method: ~A~%" method)
         (format 't "Result: ~A~%~%~%"
                 (time (distance string1 string2 :method method))))))

(defun main ()
  (if (< (length (uiop:command-line-arguments)) 2)
      (format t "Usage: levenshtein <first word> <second word>~%")
      (let ((d (distance (first (uiop:command-line-arguments))
                (second (uiop:command-line-arguments)))))
        (format t "~A~%" d)
        d)))
