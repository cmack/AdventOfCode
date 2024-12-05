(uiop:define-package #:cmack.advent-of-code.2024.day4
    (:use :cl)
    (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day4)

(defun day4 ()
  (with-open-file (in "day4-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect (coerce line 'list))))

(defun make-board (input)
  (make-array (list (length input) (length (car input)))
              :initial-contents input :element-type 'character))

(defun ntranspose (2d-array)
  "Destructively transposes square matrix 2D-ARRAY"
  (destructuring-bind (row col) (array-dimensions 2d-array)
    (loop for i below row do
      (loop for j from (1+ i) below col
            do (rotatef (aref 2d-array i j)
                        (aref 2d-array j i))))
    2d-array))

(defun transpose (2d-array)
  "Returns the transpose of square matrix 2D-ARRAY"
  (ntranspose (s:copy-array 2d-array)))

(defun stringify (2d-array)
  (destructuring-bind (row col) (array-dimensions 2d-array)
   (loop for i below row
         collect  (loop for j below col
                        collect (aref 2d-array i j) into x
                        finally (return (coerce x 'string))))))

(defun count-in-list (counter-fn list)
  (reduce #'+ (mapcar counter-fn list)))

(defun lower-diagonals (2d-array)
  "Lower left diagonals"
  (destructuring-bind (row col) (array-dimensions 2d-array)
    (loop with size = (array-total-size 2d-array)
          for i from 0 below row
          collect (loop for j = (* i col) then (+ j (1+ col))
                        while (< j size)
                        collect (row-major-aref 2d-array j)))))

(defun upper-diagonals (2d-array)
  (lower-diagonals (transpose 2d-array)))

(defun diagonals (2d-array)
  (nconc (lower-diagonals 2d-array)
         ;; remove common diagonal
         (rest (upper-diagonals 2d-array))))

(defun string-counter (string)
  (lambda (s) (s:string-count string s)))

(defun challenge-1 (input)
  (let* ((board (make-board input))
         (rboard (make-board (mapcar #'reverse input)))
         (from-left (stringify board))
         (from-right (stringify rboard))
         (from-top (stringify (transpose board)))
         (from-bottom (stringify (transpose rboard)))
         (forward-counter (string-counter "XMAS"))
         (backward-counter (string-counter "SAMX"))
         (diagonals (mapcar (lambda (s)
                              (coerce s 'string))
                            (diagonals board)))
         (rdiagonals (mapcar (lambda (s)
                               (coerce s 'string))
                             (diagonals rboard))))

    ;; format t "L:~d r:~d t:~d b:~d ld:~d lrd:~d rd:~d rrd:~d~%"
    (+
     (count-in-list forward-counter from-left)
     (count-in-list forward-counter from-right)
     (count-in-list forward-counter from-top)
     (count-in-list backward-counter from-bottom)
     (count-in-list forward-counter diagonals)
     (count-in-list backward-counter diagonals)
     (count-in-list forward-counter rdiagonals)
     (count-in-list backward-counter rdiagonals))))


(defparameter *test*
  (mapcar (lambda (s) (coerce s 'list))
          '("MMMSXXMASM"
            "MSAMXMSMSA"
            "AMXSXMAAMM"
            "MSAMASMSMX"
            "XMASAMXAMM"
            "XXAMMXXAMA"
            "SMSMSASXSS"
            "SAXAMASAAA"
            "MAMMMXMMMM"
            "MXMXAXMASX")) )

;;; Plan
;;; XMAS is four letters. The input is 140x140
;;; Lets use a 4x4 displaced array for our search window?
;;; or
;;; search forward each line
;;; search reverse each line
;;; transpose
;;; do again
