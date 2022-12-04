(uiop:define-package #:2022.day4
  (:use #:common-lisp #:serapeum))

(in-package #:2022.day4)

(defun split-dash (str)
  (split-sequence #\- str))

(defun split-comma (str)
  (split-sequence #\, str))

(defun range-ends (str)
  (mapcar #'parse-integer (split-dash str)))

(defun read-day4 (input-file)
  (with-open-file (stream input-file :direction :input)
    (loop for input = (read-line stream nil)
          while input
          collect (mapcar #'range-ends (split-comma input)))))

(defun inclusive-range-p (elves)
  (destructuring-bind ((a b) (c d)) elves
    (or (<= a c d b)
        (<= c a b d))))

(defun part1 (input-file)
  (let ((day4 (read-day4 input-file)))
    (count-if #'inclusive-range-p day4)))

(defun intersects-p (elves)
  (destructuring-bind ((a b) (c d)) elves
    (intersection (coerce (range a (1+ b)) 'list)
                  (coerce (range c (1+ d)) 'list))))

(defun part2 (input-file)
  (let ((day4 (read-day4 input-file)))
    (count-if #'intersects-p day4)))
