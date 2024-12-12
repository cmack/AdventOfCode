(uiop:define-package #:cmack.advent-of-code.2024.day11
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day11)

(defun parse-lines (input)
  (mapcar #'parse-integer (s:words input)))

(defun test (input)
  (parse-lines (first (with-input-from-string (in input)
                        (loop for line = (read-line in nil)
                              while line
                              collect line)))))

(defun day11 ()
  (parse-lines (first (uiop:read-file-lines "day11-input.txt"))))

(defparameter *test* "0 1 10 99 999")

(defun even-digits-p (stone)
  (evenp (1+ (truncate (log stone 10)))))

(defun split-number (stone)
  (mapcar #'parse-integer
          (multiple-value-list (s:halves (s:string+ stone)))))

(defun replace-stone (stone)
  (cond ((= 0 stone) (list 1))
        ((even-digits-p stone) (split-number stone))
        (t (list (* stone 2024)))))

(defun blink (input)
  (mapcan #'replace-stone input))

(defun blink-2 (stone-freqs)
  (loop
    with new-counts = (make-hash-table)
    for stone being the hash-keys of stone-freqs
      using (hash-value stone-count)
    do (cond ((= 0 stone)
              (incf (gethash 1 new-counts 0) stone-count))
             ((even-digits-p stone)
              (mapcar (lambda (split)
                        (incf (gethash split new-counts 0) stone-count))
                      (split-number stone)))
             (t
              (incf (gethash (* stone 2024) new-counts 0) stone-count)))
    finally (return new-counts)))

(defun blink-times (blinker-fn input times)
  (loop repeat times
        for x = (funcall blinker-fn input)
          then (funcall blinker-fn x)
        finally (return x)))

(defun part1 (input times)
  (length (blink-times #'blink2 input times)))

(defun part2 (input times)
  ;; start with a hash table of initial values
  ;; each iteration creates a new "frequencies" table
  (s:sum (s:hash-table-values
          (blink-times #'blink2 (s:frequencies input) times))))

;;; This challenge seems like a place for using prefix sums but the data that
;;; explodes in size is eluding my reasoning for applying s:scan.
;;; Maybe Fenwick trees? https://en.wikipedia.org/wiki/Fenwick_tree
