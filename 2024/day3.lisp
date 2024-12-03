(uiop:define-package #:cmack.advent-of-code.2024.day3
    (:use :cl)
    (:local-nicknames (:s :serapeum/bundle)
                      (:re :cl-ppcre)))

(in-package #:cmack.advent-of-code.2024.day3)

(defun day3 ()
  (with-open-file (in "day3-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun all-mul-args (lines)
  (loop for line in lines
        with scanner = (re:create-scanner "mul\\((\\d+),(\\d+)\\)")
        with result = nil
        do (re:do-register-groups ((#'parse-integer l r))
               (scanner line)
             (push (list l r) result))
        finally (return (nreverse result))))

(defun challenge-1 ()
  (reduce #'+ (s:mapply #'* (all-mul-args (day3)))))

(defun concat-day (lines)
  (apply #'s:string+ lines))

(defun collect-dos ()
  (loop
    with line = (concat-day (day3))
    for start = 0 then next-do
    for next-dont = (and start (search "don't()" line :start2 (1+ start)))
    for next-do = (and next-dont (search "do()" line :start2 (1+ next-dont)))
    while start
    collect (subseq line start next-dont)))

(defun challenge-2 ()
  (reduce #'+ (s:mapply #'* (all-mul-args (collect-dos)))))
