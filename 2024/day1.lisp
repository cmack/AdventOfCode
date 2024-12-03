(uiop:define-package #:cmack.advent-of-code.2024
    (:use :cl)
    (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024)

(defun day1 ()
  (with-open-file (in "day1-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect (mapcar #'parse-integer (s:words line)))))

(defun extract-and-sort-lists ()
  (loop for (l r) in (day1)
        collect l into l-list
        collect r into r-list
        finally (return (list (sort l-list #'<)
                              (sort r-list #'<)))))
(defun distances (l-list r-list)
  "Find distances between lists. L-LIST and R-LIST must be ordered"
  (mapcar (lambda (l r) (abs (- l r)))
          l-list r-list))

(defun distance-sum (distances)
  (reduce #'+ distances))

(defun challenge-1 ()
  (destructuring-bind (l r) (extract-and-sort-lists)
    (distance-sum (distances l r))))

(defun similarity (l-list r-list)
  (let ((r-frequencies (s:frequencies r-list)))
    (flet ((similarity-r (l)
             (* l
                (or (gethash l r-frequencies) 0))))
      (mapcar #'similarity-r l-list))))

(defun challenge-2 ()
  (destructuring-bind (l r) (extract-and-sort-lists)
    (distance-sum (similarity l r))))
