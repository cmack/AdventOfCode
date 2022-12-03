(uiop:define-package #:2022.day3
  (:use #:common-lisp #:serapeum))

(in-package #:2022.day3)

(defparameter *demo* (list
                      "vJrwpWtwJgWrhcsFMMfFFhFp"
                      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
                      "PmmdzqPrVvPwwTWBwg"
                      "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
                      "ttgJtRGJQctTZtZT"
                      "CrZsJsPPZsGzwwsLwLmpwMDw"))

(defparameter *letter-scores*
  (map 'list #'cons
       "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
       (range 1 53)))

(defun read-rucksacks (input-file)
  (with-open-file (stream input-file :direction :input)
    (loop for input = (read-line stream nil)
          while input
          collect input)))

(defun halve (seq)
  (batches seq (/ (length seq) 2) :even t))

(defun score-letter (char)
  (assocdr char *letter-scores*))

(defparameter *day3* (read-rucksacks "day3-input"))

(defun part1 (rucksacks)
  (loop for sack in rucksacks
        for (comp1 comp2) = (halve (coerce sack 'list))
        for common = (first (intersection comp1
                                          comp2
                                          :test #'char=))
        summing (score-letter common)))

(defun part2 (rucksacks)
  (loop for triad in (batches rucksacks 3)
        summing (score-letter (first (reduce #'intersection
                                             (mapcar (lambda (str)
                                                       (coerce str 'list))
                                                     triad))))))
