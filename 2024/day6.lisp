(uiop:define-package #:cmack.advent-of-code.2024.day6
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day6)

(defun test (input)
  (with-input-from-string (in input)
    (loop for line = (read-line in nil)
          while line
          collect line into lines
          finally (return (coerce lines 'vector)))))

(defun day6 ()
  (with-open-file (in "day6-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect line into lines
          finally (return (coerce lines 'vector)))))

(defun find-start (board)
  (loop for i from 0 below (length board)
        for guard = (search "^" (svref board i) :test #'string=)
        when guard
          return (list guard i)))

(defun find-obstacle-right (board start)
  (destructuring-bind (x y) (first (last start))
    (let ((hit (search "#" (aref board y)
                       :test #'string= :start2 (1+ x))))
      (if hit
          (list (1- hit) y)
          (list (length board) y)))))

(defun find-obstacle-left (board start)
  (destructuring-bind (x y) (first (last start))
    (let ((hit (search "#" (aref board y)
                       :test #'string= :end2 (1- x) :from-end t)))
      (if hit
          (list (1+ hit) y)
          (list -1 y)))))

(defun find-obstacle-up (board start)
  (destructuring-bind (x y) (first (last start))
    (let ((hit (loop
                 for i from y downto 0
                 for row = (aref board i)
                 when (char= #\# (aref row x))
                   return (1+ i))))
      (list x (or hit -1)))))

(defun find-obstacle-down (board start)
  (destructuring-bind (x y) (first (last start))
    (let ((hit (loop
                 for i from y below (length board)
                 for row = (aref board i)
                 when (char= #\# (aref row x))
                   return (1- i))))
      (list x (or hit (length board))))))

(defun next-direction(fn)
  (let ((next (s:alist-hash-table
               `((,#'find-obstacle-up . ,#'find-obstacle-right)
                 (,#'find-obstacle-right . ,#'find-obstacle-down)
                 (,#'find-obstacle-down . ,#'find-obstacle-left)
                 (,#'find-obstacle-left . ,#'find-obstacle-up)))))
    (gethash fn next #'find-obstacle-up)))

(defun expand (path next-stop)
  ;; yuck
  (destructuring-bind (x1 y1) (first (last path))
    (destructuring-bind (x2 y2) next-stop
      (let* ((dy (- y2 y1))
             (dx (- x2 x1))
             (n (1+ (max (abs dy) (abs dx)))))
        (append path
                (mapcar #'list
                        (s:iota n :start x1 :step (signum dx))
                        (s:iota n :start y1 :step (signum dy))))))))
(defun expand-all (path)
  (remove-duplicates (reduce #'expand path
                             :start 1
                             :initial-value (list (first path)))
                     :test #'equal))

(defun in-bounds-p (board next)
  (every (lambda (x)
           (< -1 x (length board)))
         next))

(defun clamp-to-board (board next)
  (mapcar (lambda (x)
            (s:clamp x 0 (1- (length board))))
          next))

(defun run-board (board)
  (labels ((runner (start direction-fn)
             (let ((next (funcall direction-fn board start)))
               (print start)
               (if (in-bounds-p board next)
                   (runner (append start (list next))
                           (next-direction direction-fn))
                   (expand-all
                    (append start
                            (list (clamp-to-board board next))))))))
    (runner (list (find-start board)) #'find-obstacle-up)))

(defun challenge-1 (input)
  (length (run-board input)))

(defparameter *test* "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")


;;; Ideas: It's tempting to start with a 2D array but I found from previous
;;; days that CL's searching within a 2D array isn't so nice.  I think I'll
;;; stick with a vector of strings and use SEARCH for determining
;;; position. This will work ok for horizontal position. Maybe I'll consider a
;;; transpose for vertical movement/search
