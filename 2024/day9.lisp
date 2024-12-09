(uiop:define-package #:cmack.advent-of-code.2024.day9
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day9)

(defun numchar->num (numchar)
  (- (char-int numchar) #.(char-int #\0)))

(defun parse-lines (input)
  (mapcar (lambda (l)
            (mapcar #'numchar->num (coerce l 'list)))
          input))

(defun test (input)
  (parse-lines (with-input-from-string (in input)
                 (loop for line = (read-line in nil)
                       while line
                       collect line))))

(defun expand (input)
  (let ((expansion
          (loop
            for (file space) on input by #'cddr
            for id from 0
            nconc (nconc (make-list file :initial-element id)
                         (when space (make-list space :initial-element nil))))))
    (coerce expansion 'vector)))

(defun compact (input)
  (loop for i below (length input)
        for f = (elt input i)
        for end-file = (position-if #'numberp input :from-end t)
        until (= i end-file)
        when (null f)
          do (rotatef (elt input i) (elt input end-file))
        finally (return input)))

(defun checksum (compacted)
  (let ((disk (delete nil compacted)))
    (reduce #'+ (map 'vector #'* disk (s:iota (length disk))))))

(defun day9 ()
  (first (parse-lines (uiop:read-file-lines "day9-input.txt"))))

(defparameter *test* "2333133121414131402")


(defun part1 (input)
  (checksum (compact (expand input))))
