(uiop:define-package #:cmack.advent-of-code.2024.day7
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day7)

(defun parse-lines (input)
  (mapcar (s:curry #'mapcar #'parse-integer)
          (mapcar #'s:words input)))

(defun test (input)
  (parse-lines (with-input-from-string (in input)
                 (loop for line = (read-line in nil)
                       while line
                       collect line))))

(defun day7 ()
  (parse-lines (uiop:read-file-lines "day7-input.txt")))

(defparameter *original-ops* nil) ;;

(defun op-graph (input)
  (labels ((scan-next (args)
             (list* (first args)
                    (last *original-ops* (1- (length args)))))

           (scan-mul (args &optional (target nil))
             (if (< (length args) 2)
                 args
                 (let* ((next (scan-next args))
                        (next-sums (s:scan #'+ next))
                        (next-muls (s:scan #'* next)))
                   (cond
                     ((or (= target (s:lastcar next-sums))
                          (= target (s:lastcar next-muls)))
                      (list target))
                     (t (list (cdr args)
                              (list (scan-mul (cdr next-sums) target)
                                    (scan-mul (cdr next-muls) target))))))))
           (scan-tree (op-args test)
             (list test (scan-mul op-args test))))

    (loop for (test . *original-ops*) in input
          collect (scan-tree *original-ops* test))))



;;; we can tell how many to splice from length of arg list
(defun challenge-1 (input)
  (loop for (test . tree) in (op-graph input)
        when (member test (s:flatten tree))
          ;collect tree
          sum test

        ))

;; 6083020304082 too high
;; 6083020304036 just right... OMG that was a hard thinko to track and fix

;;; Ideas
;;; 1. map the #'* prefix sum and if the final is too little -> exit | NOPE sums before muls can get to the mark
;;; 2. form rest of binary tree for the #'+ and #'* prefix sums
;;; 3. traverse and find member who #'= total.
;;; 4. How to bail early if we know the total can't be composed?
;;; 4.1  Prune any item greater than the total.

(defparameter *test* "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")
