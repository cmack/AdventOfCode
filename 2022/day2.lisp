(uiop:define-package #:2022.day2
  (:use #:common-lisp #:serapeum))

(in-package #:2022.day2)

(defparameter *scores* '((rock . 1)
                         (paper . 2)
                         (scissors . 3)))

(defparameter *rock* '((rock . 3)
                       (paper . 0)
                       (scissors . 6)))

(defparameter *paper* '((rock . 6)
                        (paper . 3)
                        (scissors . 0)))

(defparameter *scissors* '((rock . 0)
                           (paper . 6)
                           (scissors . 3)))

(defun input-to-game-throw (input)
  (case input
    ((A X) 'rock)
    ((B Y) 'paper)
    ((C Z) 'scissors)))

(defun read-games ()
  (with-open-file (stream "day2-input" :direction :input)
    (loop for input = (read-line stream nil)
          while input
          collect (mapcar-into #'alexandria:symbolicate
                               (split-sequence " " input :test #'string=)))))

(defun score (shape alist)
  (rest (assoc shape alist)))

(defun shape-score (shape)
  (score shape *scores*))

(defun outcome-score (them me)
  (case me
    (rock (score them *rock*))
    (paper (score them *paper*))
    (scissors (score them *scissors*))))

(defun game-score (them me)
  (+ (shape-score me) (outcome-score them me)))

(defun part1 ()
  (loop for (them me) in (read-games)
        summing (game-score (input-to-game-throw them)
                            (input-to-game-throw me))))

(defparameter *wins* '((rock . scissors)
                       (paper . rock)
                       (scissors . paper)))

(defun win (them)
  (first (rassoc them *wins*)))

(defun lose (them)
  (rest (assoc them *wins*)))

(defun strategy (input)
  (ecase input
    (X #'lose)
    (Y #'identity)
    (Z #'win)))

(defun part2 ()
  (loop for (their-input strategy) in (read-games)
        for their-throw = (input-to-game-throw their-input)
        summing (game-score their-throw
                            (funcall (strategy strategy) their-throw))))
