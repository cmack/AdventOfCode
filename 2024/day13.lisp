(uiop:define-package #:cmack.advent-of-code.2024.day13
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle) (:regex :cl-ppcre)))

(in-package #:cmack.advent-of-code.2024.day13)

(defun arrayify (input)
  (make-array (list (length input) (length (first input)))
              :initial-contents input))

(defun parse-lines (input)
  (s:split-sequence "" input :test #'string=))

(defun test (input)
  (parse-lines (with-input-from-string (in input)
                 (loop for line = (read-line in nil)
                       while line
                       collect line))))
(defun button-regex (str)
  (regex:register-groups-bind ((#'parse-integer x y))
      ("X[\\+=](\\d+), Y[\\+=](\\d+)" str)
    (complex x y)))

(defun complexify (input)
  (mapcar (lambda (claw) (mapcar #'button-regex claw))
          input))

(defun day13 ()
  (parse-lines (uiop:read-file-lines "day13-input.txt")))

(defun b-solve (a button-a button-b goal)
  (/ (- goal (* a button-a))
     button-b))

(defun mash-buttons (button-a button-b goal)
  (loop
    repeat 100
    for a from 0
    for b = (b-solve a button-a button-b goal)
    until (and (integerp a) (integerp b))
    finally (return (and (realp a) (realp b) (list a b)))))

(defun solve (button-a button-b goal)
  ;; Why did I do this to myself? I should have done the math:
  ;; A(x+yi) + B(j+ki) = n + mi
  ;; x+yi => button-a
  ;; j+ki => button-b
  ;; n+mi => goal
  (let ((x (realpart button-a))
        (y (imagpart button-a))
        (j (realpart button-b))
        (k (imagpart button-b))
        (n (realpart goal))
        (m (imagpart goal)))
    ;; Algebraically solve for B in the real parts Ax+Bj = n
    ;; Sub back in for B in the imaginary parts Ay+Bk = m
    (let* ((A (/ (- (* m j) (* n k))
                 (- (* y j) (* x k))))
           (B (/ (- n (* A x))
                 j)))
      (when (and (integerp A) (integerp B))
        (list A B)))))


(defparameter *test* "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defun sum-tokens (solved-input)
  (s:sum
   (s:mapcar-into #'s:sum
                  (mapcar (s:partial #'mapcar #'* '(3 1))
                          (remove nil solved-input)))))
(defun part1 (input)
  (sum-tokens (s:mapply #'mash-buttons (complexify input))))

(defun increase-goal (input)
  (loop for (button-a button-b goal) in (complexify input)
        collect (list button-a button-b (+ #c(10000000000000 10000000000000) goal))))

(defun part2 (input)
  (sum-tokens (s:mapply #'solve (increase-goal input))))
