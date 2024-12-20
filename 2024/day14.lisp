(uiop:define-package #:cmack.advent-of-code.2024.day14
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)
                    (:regex :cl-ppcre)))

(in-package #:cmack.advent-of-code.2024.day14)

(defun button-regex (str)
  (regex:register-groups-bind ((#'parse-integer px py vx vy))
      ("p=(-?\\d+),(-?\\d+) v=(-?\\d+),(-?\\d+)" str)
    (list (complex px py) (complex vx vy))))

(defun roboticize (input)
  (mapcar #'button-regex input))

(defun test (input)
  (with-input-from-string (in input)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun day14 ()
  (uiop:read-file-lines "day14-input.txt"))

(defparameter *test* "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")


;;; Count robots in each quadrant of a 100 x 100 grid
;;; multiply for the safety factor.
;;; movement wraps around the board. Robots can be in same space
;;;

(defun quadrant-counts (positions grid-dim)
  ;; Any robot in the middle doesn't count
  (destructuring-bind (grid-w grid-h) grid-dim
    (let ((x-half (truncate grid-w 2))
          (y-half (truncate grid-h 2)))
      (flet ((quadrant (pos)
               (destructuring-bind (px py) pos
                 (cond ((< -1 px x-half)
                        (cond ((< -1 py y-half) :q1)
                              ((< y-half py grid-h) :q3)))

                       ((<  x-half px grid-w)
                        (cond ((< -1 py y-half) :q2)
                              ((< y-half py grid-h) :q4))))))
             (crosshair-p (x)
               (or (= x-half (first x))
                   (= y-half (second x)))))
        (mapcar #'length
                (remove-if #'crosshair-p
                           (s:assort positions :key #'quadrant :test #'eq)
                           :key #'first))))))

(defun safety-factor (positions grid-dim)
  (s:prod (quadrant-counts positions grid-dim)))

(defun translate-robot (position velocity time dimensions)
  (destructuring-bind (grid-x grid-y) dimensions
    (let ((translated (+ position (* velocity time))))
      (list (mod (realpart translated) grid-x)
            (mod (imagpart translated) grid-y)))))

(defun part1 (input grid-dims)
  (let ((robots (roboticize input)))
    (safety-factor (mapcar (lambda (robot)
                             (destructuring-bind (pos vel) robot
                               (translate-robot pos vel 100 grid-dims)))
                           robots)
                   grid-dims)))

;;; (part1 (day14) '(101 103))

(defun part2 (input grid-dims)
  (declare (optimize (debug 3)))
  (loop
    with robots = (roboticize input)
    for time from 1 to 10000
    for positions = (mapcar (lambda (robot)
                              (destructuring-bind (pos vel) robot
                                (translate-robot pos vel time grid-dims)))
                            robots)
    for grid = (make-array grid-dims :initial-element ".")
    when (and (> (length (remove-duplicates positions :test #'equal))
                 495)
              (> (count-if (lambda (pos)
                             (destructuring-bind (x y) pos
                               (and (< 25 x 80)
                                    (< 25 y 80))))
                           positions)
                 250))
      do (loop for (x y) in positions
               do (setf (aref grid x y) "#")
               finally (format t "~%~%~%--------- ~d ---------~%~a~%"
                               time grid))))
