(uiop:define-package #:2022.day1
  (:use #:common-lisp #:serapeum))

(in-package #:2022.day1)

(defun total-calories (calorie-list)
  (reduce #'+ calorie-list))

(defun max-calorie-carrier (elves)
  (reduce #'max (mapcar #'total-calories elves)))

(defun read-elves ()
  (with-open-file (stream "day1-input" :direction :input)
    (split-sequence nil
                    (loop for input = (read-line stream nil)
                          while input
                          collect (parse-integer input :junk-allowed t)))))

(defun day1 ()
  (let ((elves (read-elves)))
    (max-calorie-carrier elves)))


(defun day1-2 ()
  (let* ((elves (read-elves))
         (sorted-calorie-carriers (sort (mapcar #'total-calories elves) #'>)))
    (apply #'+ (take 3 sorted-calorie-carriers))))
