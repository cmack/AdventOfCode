(uiop:define-package #:cmack.advent-of-code.2024.day2
    (:use :cl)
    (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day2)


(defun day2 ()
  (with-open-file (in "day2-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect (mapcar #'parse-integer (s:words line)))))

(defun safe-epsilon-p (delta)
  (<= 1 (abs delta) 3))

(defun safe-p (deltas)
  (and (or (every #'plusp deltas)
           (every #'minusp deltas))
       (every #'safe-epsilon-p deltas)))

(defun deriv (levels)
  (mapcar #'- (rest levels) levels))

(defun challenge-1 ()
  (loop for deltas in (mapcar #'deriv (day2))
        counting (safe-p deltas) into safes
        finally (return safes)))

(defun challenge-1-functional (levels)
  (length (s:partition #'safe-p (mapcar #'deriv levels))))

(defun levels-safe-p (levels)
  (safe-p (deriv levels)))

;;; Experiment with L2 error for another time.
;; (defun l1 (y1 y2)
;;   (- y1 y2))

;; (defun l2 (y1 y2)
;;   (expt (- y1 y2) 2))

;; (defun constant-l2 (expected)
;;   (lambda (y1)
;;     (l2 y1 expected)))

;; (defun error-level (deltas error-fn)
;;   (/ (reduce #'+ (mapcar error-fn deltas))
;;      (float (length deltas))))

;; (defun l1-error (deltas)
;;   (error-level deltas (lambda (y1) (l1 y1 1))))

;; (defun l2-error (deltas)
;;   (error-level deltas (constant-l2 1)))

;; (defun increasing-delta-p (deltas)
;;   (plusp (first deltas)))

(defun dampened-safe-p (levels)
  (loop for i from 1 upto (length levels)
        for removed-1 = (nconc (butlast levels i)
                               (rest (last levels i)))
         thereis (levels-safe-p removed-1)))

(defun challenge-2 (levels)
  (multiple-value-bind (safe unsafe) (s:partition #'levels-safe-p levels)
    (+ (length safe)
       (count-if #'dampened-safe-p unsafe))))
