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

;; (loop
;;   for line in lines
;;   collect (re:regex-replace-all "don't\\(\\).*\\:?do\\(\\)" line "|---do()------|"))

(defun remove-donts (lines)
  (loop
    for line in lines
        ;; (?:do\(\)[\w!?"#$%&'()=~^\\\/,.<>;+:*\]}\[{|])*(?<!don't\(\)[\w()^,+\]\[&"!\/\/%|~=*]+)mul\((\d+),(\d+)\)
        ;; no variable length neg. look behind T_T
        with regex =  "don't.+(?=do\\(\\))+.*(?:mul\\((\\d+),(\\d+)\\))+(?!:don't\\(\\))"
  ;; with regex = "(?:mul\\((\\d+),(\\d+)\\).*)+don't\\(\\).*(?!mul)*(?=do\\(\\))*.*(?:mul\\((\\d+),(\\d+)\\))+"
        ;; "(?:do\\(\\)[\\w!?\"#$%&'()=~^\\\\\/,.<>;+:*\\]}\\[{|-])*(?<!don't\\(\\)[\\w!?\"#$%&'()=~^\\\\\/,.<>;+:*\\]}\\[{|-]+)mul\\((\\d+),(\\d+)\\)"
    collect (re:regex-replace-all "don't\\(\\).*(?=do\\(\\))*$" line "")))

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

;;; I absolutely spent a childishly obstinate amount of time on getting a
;;; regex that would work. I thought I might learn something new about
;;; regexes. In a way I did... I learned the semicolons can ruin your matching
;;; and that CL-PPCRE doesn't handle all negative lookbehind cases. Also, any
;;; confidence I had in reading regexes is ruined by the amount of escaping
;;; necessary. I should have bailed much earlier for the do-while loop.
;;; Leaving this shame for commit history

;; (defun all-do-mul-args (lines)
;;   (loop
;;     for line in lines
;;     for do-its = (re:regex-replace-all "don't\\(\\).*(?=do\\(\\))" line "")
;;     with regex = "mul\\((\\d+),(\\d+)\\)"
;;     with scanner = (re:create-scanner regex)
;;     with result = nil
;;     do (re:do-register-groups ((#'parse-integer l r))
;;            (scanner do-its)
;;          (push (list l r) result))
;;     finally (return (nreverse result))))

;; (defun test ()
;;   (loop for line in (day3)
;;         for do-its = (re:regex-replace-all "don't\\(\\).*(?=do\\(\\))" line "")
;;         collect do-its))

;; X 4372180
