(uiop:define-package #:cmack.advent-of-code.2024.day11
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day11)

(defun parse-lines (input)
  (mapcar #'parse-integer (s:words input)))

(defun test (input)
  (parse-lines (first (with-input-from-string (in input)
                        (loop for line = (read-line in nil)
                              while line
                              collect line)))))

(defun day11 ()
  (parse-lines (first (uiop:read-file-lines "day11-input.txt"))))

(defparameter *test* "0 1 10 99 999")

(defun even-digits-p (stone)
  (evenp (1+ (truncate (log stone 10)))))

(defun split-number (stone)
  (mapcar #'parse-integer
          (multiple-value-list (s:halves (s:string+ stone)))))

(defun replace-stone (stone)
  (cond ((= 0 stone) (list 1))
        ((even-digits-p stone) (split-number stone))
        (t (list (* stone 2024)))))

(defun blink (input)
  (mapcan #'replace-stone input))

(defun part1 (input times)
  (loop repeat times
        for x = (blink input) then (blink x)
        finally (return (length x))))
