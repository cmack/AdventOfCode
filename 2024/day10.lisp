(uiop:define-package #:cmack.advent-of-code.2024.day10
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day10)

(defun numchar->num (numchar)
  (- (char-int numchar) #.(char-int #\0)))

(defun arrayify (input)
  (make-array (list (length input) (length (first input)))
              :initial-contents input))

(defun parse-lines (input)
  (mapcar (lambda (l)
            (mapcar #'numchar->num (coerce l 'list)))
          input))

(defun test (input)
  (arrayify
   (parse-lines
    (with-input-from-string (in input)
      (loop for line = (read-line in nil)
            while line
            collect line)))))

(defun day10 ()
  (arrayify (parse-lines (uiop:read-file-lines "day10-input.txt"))))

(defparameter *test* "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")

(defun in-bounds-p (topog coord)
  (destructuring-bind (w h) (array-dimensions topog)
    (and (< -1 (first coord) w)
         (< -1 (second coord) h))))

(macrolet ((defmoves (direction delta)
             `(defun ,(s:symbolicate "LOOK-" direction) (topog current)
                (let ((,direction (mapcar #'+ current ,delta)))
                  (when (in-bounds-p topog ,direction)
                    (list (apply #'aref topog ,direction) ,direction))))))
  (defmoves north (list -1 0))
  (defmoves south (list 1 0))
  (defmoves west (list 0 -1))
  (defmoves east (list 0 1)))

(defun successors (topog current)
  (declare (optimize (debug 3)))
  (flet ((look-next-move (fn coord)
           (funcall fn topog (second coord)))
         (illegal-next-p (next)
           (or (null next)
               (and (consp next)
                    (/= 1 (- (first next) (first current)))))))
    (let ((nexts (remove-if #'illegal-next-p
                            (mapcar #'look-next-move
                                    (list #'look-north #'look-south
                                          #'look-west #'look-east)
                                    (make-list 4 :initial-element current)))))
      (if (= 8 (first current))
          nexts
          (mapcan (s:partial #'successors topog) nexts)))))

(defun count-terminating (topog start)
  (length (remove-duplicates (successors topog start) :test #'equal)))

(defun find-trailheads (input)
  (destructuring-bind (h w) (array-dimensions input)
    (loop for i below h
          nconc (loop for j below w
                      for val = (aref input i j)
                      when (= val 0)
                        collect (list val (list i j))))))


(defun part1 (input)
  (mapcar (s:partial #'count-terminating input)
          (find-trailheads input)))

(defun count-possible-terminating (topog start)
  (length (successors topog start)))

(defun part2 (input)
  (s:sum (mapcar (s:partial #'count-possible-terminating input)
                 (find-trailheads input))))
