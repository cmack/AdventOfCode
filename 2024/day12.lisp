(uiop:define-package #:cmack.advent-of-code.2024.day12
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day12)

(defun arrayify (input)
  (make-array (list (length input) (length (first input)))
              :initial-contents input))

(defun parse-lines (input)
  (mapcar (lambda (l)
            (coerce l 'list))
          input))

(defun test (input)
  (arrayify (parse-lines (with-input-from-string (in input)
                           (loop for line = (read-line in nil)
                                 while line
                                 collect line)))))

(defun day12 ()
  (arrayify (parse-lines (uiop:read-file-lines "day12-input.txt"))))

(defparameter *test* "AAAA
BBCD
BBCC
EEEC")

(defparameter *2nd-test* "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

(defparameter *larger-test* "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

;; Reading: feels like a convex hull type problem
;; or does this feel like a voronoi type problem?
;; or k-nearest neighbor?


(defun fence-price (area borders)
  (* area borders))

(defun in-bounds-p (map coord)
  (destructuring-bind (w h) (array-dimensions map)
    (and (< -1 (realpart coord) w)
         (< -1 (imagpart coord) h))))

(defun complex->index (complex)
  (list (imagpart complex) (realpart complex)))

;; collect the faces
;; create the edges
(macrolet ((defmoves (direction delta)
             `(defun ,(s:symbolicate "LOOK-" direction) (map current)
                (let ((,direction (+ current ,delta)))
                  (when (in-bounds-p map ,direction)
                    (list (apply #'aref map (complex->index ,direction))
                          ,direction))))))
  (defmoves north #C(0 -1))
  (defmoves south #C(0 1))
  (defmoves west #C(-1 0))
  (defmoves east #C(1 0)))

(defun face-neighbors (map face)
  (declare (optimize (debug 3)))
  (mapcar (lambda (lookfn) (funcall lookfn map face))
          (list #'look-west #'look-north #'look-east #'look-south)))

(defun group-contiguous (map start)
  (let ((visited ())
        (perimeter-count 0)
        (entry (list (aref map (imagpart start) (realpart start))
                     start)))
    (labels
        ((visited-p (x)
           (member x visited :test #'equal))
         (unvisited (seq)
           (set-difference seq visited :test #'equal))
         (group-next (start)
           (if (visited-p start)
               nil
               (destructuring-bind (letter coord) start
                 (multiple-value-bind (outside inside)
                     (s:partition (lambda (x)
                                    (or (null x)
                                        (char/= (first x) letter)))
                                  (face-neighbors map coord))
                   (progn (pushnew start visited :test #'equal)
                          (incf perimeter-count (length outside))
                          (list* start
                                 (remove nil
                                         (mapcar #'group-next
                                                 (unvisited inside))))))))))
      (values (group-next entry)
              (length visited)
              perimeter-count))))

(defun flatten-region (region)
  (multiple-value-bind(letters areas)
      (s:partition #'characterp (remove-duplicates (s:flatten region)))
    (acons (first letters) areas nil)))

(defun add-visited-region (visited-table region)
  (let ((visited-areas (flatten-region region)))
    (loop for area in (cdar visited-areas)
          with letter = (caar visited-areas)
          do (setf (gethash area visited-table) letter)
          finally (return visited-table))))

(defun faces (map)
  (loop with regions = (make-hash-table)
        with visited = (make-hash-table)
        for x below (array-dimension map 0)
        do (loop for y below (array-dimension map 1)
                 for region-letter = (aref map y x)
                 unless (gethash (complex x y) visited)
                   do (multiple-value-bind (region areas perimeters)
                          (group-contiguous map (complex x y))
                        (add-visited-region visited region)
                        (push (list (remove-duplicates (s:flatten region))
                                    areas
                                    perimeters)
                              (gethash region-letter regions))))
        finally (return regions)))

(defun fence-prices (map)
  (s:maphash-return (lambda (letter regions)
                      (loop for (regions areas perimeters) in regions
                            sum (fence-price areas perimeters)))
                    (faces map)))

(defun part1 (input)
  (reduce #'+ (fence-prices input)))
