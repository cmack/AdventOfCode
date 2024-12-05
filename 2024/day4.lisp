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

;;; Nevermind about displaced arrays. I need "conformally displaced arrays"
;;; which was only part of LispMs and not CL.
;;; Next idea, find each #\A position, then search around for SAM and MAS

(defun 2D-search (2d-array string)
  (let ((search-vec (make-array (array-total-size 2d-array)
                                :element-type 'character
                                :displaced-to 2d-array)))
    (loop for i below (length search-vec)
          for start = 0 then (1+ next)
          for next = (search string search-vec :start2 start)
          while next
          collect next)))

(defun A-positions (2d-array)
  (let* ((dim (array-dimension 2d-array 0))
        (dim-1 (1- dim)))
    (flet ((on-border-p (index)
             (multiple-value-bind (row col) (floor index dim)
               (or (zerop col)
                   (zerop row)
                   (= col dim-1)
                   (= row dim-1)))))
      (remove-if #'on-border-p
                 (2D-search 2d-array "A")))))

(defun x-vals (board index)
  (let ((dim (array-dimension board 0)))
    (list (row-major-aref board (- index dim 1))
          (row-major-aref board (- index dim -1))
          (row-major-aref board (+ index dim -1))
          (row-major-aref board (+ index dim 1)))))

(defun winner-p (x-val)
  (flet ((mas-p (list)
           (null (set-difference '(#\S #\M) list))))
    (destructuring-bind (tl tr bl br) x-val
      (and (mas-p (list tl br))
           (mas-p (list tr bl))))))

(defun challenge-2 (input)
  (loop with board = (make-board input)
        for a in (A-positions board)
        counting (winner-p (x-vals board a))))
