(uiop:define-package #:cmack.advent-of-code.2024.day8
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day8)

(defun parse-lines (input)
  (mapcar (lambda (x) (coerce x 'list)) input))

(defun test (input)
  (parse-lines (with-input-from-string (in input)
     (loop for line = (read-line in nil)
           while line
           collect line))))

(defun day8 ()
  (parse-lines (uiop:read-file-lines "day8-input.txt")))

;;; frequency indicated by single {lower|upper}case letter or digit.
;;; antinodes at any point that is perfectly in line with two antennas of the same frequency but only when one of the antennas is twice as far away as the other.
;;;   -> for any pair of antennas with same freq. there are 2 antinodes
;;;  so if distance between 2 antennas of same freq. is 2x there are antinodes. Each antennas is the midpoint of the other antenna and the anti node.


;;; Find all unique antennas and their coordinates.
;;; Pair up the ones that are 2x
(defstruct (antenna (:type list))
  (frequency #\. :type character)
  (coord #C(0 0) :type number))

(defun norm (complex)
  (sqrt (+ (expt (realpart complex) 2)
           (expt (imagpart complex) 2))))

(defmethod slope ((a1 number) (a2 complex))
  (- a2 a1))

(defmethod slope ((a1 complex) (a2 number))
  (- a2 a1))

(defmethod slope ((a1 list) (a2 list))
  (slope (antenna-coord a2)
         (antenna-coord a1)))

(defun distance (a1 a2)
  (norm (slope a1 a2)))

(defun gather-antennas (input)
  (s:assort
   (loop for y below (length input)
         nconc (loop for x from 0
                     for freq in (elt input y)
                     unless (char= freq #\.)
                       collect (make-antenna :frequency freq
                                             :coord (complex x y))))
   :key #'antenna-frequency))

(defun antipole (a1 a2)
  (+ (antenna-coord a2)
     (slope a2 a1)))

(defun gather-antipoles (antenna-list)
  "Antipoles of same-frequency antennas"
  (let ((antipole-set ()))
    (flet ((add-antipoles (antenna-pair)
             (setf antipole-set (adjoin (antipole (first antenna-pair)
                                                  (second antenna-pair))
                                        antipole-set
                                        :test #'=))))
      (s:map-permutations #'add-antipoles antenna-list :length 2)
      antipole-set)))

(defun c<= (c1 c2)
  (and (<= (realpart c1) (realpart c2))
       (<= (imagpart c2) (imagpart c1))))

(defun out-of-bounder (size)
  (lambda (x)
    (not (and (c<= x size)
              (c<= (complex 0 size) x)))))

(defun part1 (input)
  (let ((size (length input)))
    (remove-if (out-of-bounder (1- size))
               (reduce #'union
                       (mapcar #'gather-antipoles (gather-antennas input))))))

(defun part2 (input)
  )

(defparameter *test* "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")


;;; 250 too high
;;; 240..
