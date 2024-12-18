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

(defparameter *e* "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")

(defparameter *t2* "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA")

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
  (defmoves east #C(1 0))
  (defmoves northeast #C(1 -1))
  (defmoves southeast #C(1 1))
  (defmoves southwest #C(-1 1))
  (defmoves northwest #C(-1 -1)))

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

(defun square-neighbors (map face)
  (mapcar (lambda (lookfn) (funcall lookfn map face))
          (list #'look-west #'look-northwest #'look-north #'look-northeast
                #'look-east #'look-southeast #'look-south #'look-southwest)))

(defun 2x2-march (map face)
  (destructuring-bind (w nw n ne e se s sw) (square-neighbors map face)
    (let ((c (list (apply #'aref map (complex->index face))
                   face)))
      (list (list c w nw n)
            (list e c n ne)
            (list se s c e)
            (list s sw w c)))))

(defun score-corners (2x2 matcher)
  (loop
    for k from 0
    for f in 2x2
    when (funcall matcher f)
      sum (expt 2 k) into j
    finally (return (case j
                      ;; my god is this fizzbuzz in the wild...
                      ;; I'm being explicit anyway.
                      ;; empty diagonals worth 2
                      ((5 10) 2)
                      ;; empty, full, vertical, or horizontal worth 0
                      ((0 3 6 9 12 15) 0)
                      ;; all others are 1
                      (t 1)))))

(defun score-region (map region letter)
  ;; counts the corners which is the same as straight fence segments
  (declare (optimize (debug 3)))
  (let ((faces (remove-if #'characterp region)))
    (loop with seen = (make-hash-table :test #'equal)
          for coord in faces
          do (loop
               for square in (2x2-march map coord)
               unless (gethash square seen)
                 do (setf (gethash square seen)
                          (score-corners square (lambda (f)
                                                  (and f
                                                       (char= (first f) letter)
                                                       (member (second f) faces))))))
          finally (return (s:sum (s:hash-table-values seen))))))

(defun straight-fence-prices (map)
  (s:maphash-return (lambda (letter regions)
                      (loop for (region areas perimeters) in regions
                            sum (fence-price areas (score-region map region letter))))
                    (faces map)))

(defun part2 (input)
  (s:sum (straight-fence-prices input)))


;;; Hmm for part 2 could I maximize the count of Xs and Ys in a given region?
;; if I had kept my perimeter faces this could have been easier.  Ugh. I think
;; I could have used the rectangle principle for part 1 and it would've been
;; so much quicker
;; Hindsight: nope, this thought was all wrong


;; For part2 -- counting spots / faces between corners. Starting to feel like raster ops
;; on the grid directly would be smarter?


;;; 867130 is too high
;;; 865662 winner
