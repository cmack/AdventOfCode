(uiop:define-package #:cmack.advent-of-code.2024.day5
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day5)

(defun test (input)
  (with-input-from-string (in input)
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun day5 ()
  (with-open-file (in "day5-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect line)))

(defun split-rules (input)
  "Returns rules and orderings as multiple values"
  (multiple-value-bind (rules orderings)
      (s:partition (s:curry #'s:string-contains-p "|") input)
    (values rules
            (remove "" orderings :test #'string=))))

(defun parse-rules (rules)
  (mapcar (s:curry #'mapcar #'parse-integer)
          (mapcar (lambda (rule)
                    (s:split-sequence "|" rule :test #'string=))
                  rules)))

(defun make-orderings (orderings)
  (mapcar (s:curry #'mapcar #'parse-integer)
          (mapcar #'s:words orderings)))

(defun order (ordering sorter-fn)
  (sort (copy-seq ordering) sorter-fn))

(defun ordered-p (ordering sorter-fn)
  (equal ordering (order ordering sorter-fn)))

(defun middle-elt (list)
  (assert (oddp (length list)))
  (elt list (floor (length list) 2)))

(defun parsed-input (input)
  (multiple-value-bind (raw-rules raw-orderings) (split-rules input)
    (values (parse-rules raw-rules)
            (make-orderings raw-orderings))))

(defun toposorter (ordering rules)
  ;; Only make the sorter for the rules in the ordering.
  (let ((narrowed-rules
          (remove-if-not (lambda (rule) (member rule ordering))
                         rules :key #'car)))
    (s:toposort narrowed-rules)))

(defun challenge-1 (input)
  (multiple-value-bind (rules orderings) (parsed-input input)
    (reduce #'+
            (mapcar #'middle-elt
                    (s:filter (lambda (x)
                                (ordered-p x (toposorter x rules)))
                              orderings)))))

(defun challenge-2 (input)
  (multiple-value-bind (rules orderings) (parsed-input input)
    (let* ((incorrect (remove-if (lambda (x) (ordered-p x (toposorter x rules)))
                                 orderings)))
      (reduce #'+
              (mapcar #'middle-elt
                      (mapcar (lambda (x)
                                (order x (toposorter x rules)))
                              incorrect))))))

;;; initial ideas: this feels like a priority queue problem. Try it out?  Next
;;; idea, enclose the rule as a lambda in a hash table, then apply all rules
;;; with EVERY

(defparameter *test* "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")
