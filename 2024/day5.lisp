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

(defun assoc-rules (parsed-rules)
   (s:assort parsed-rules :key #'first))

(defun make-rules (rules)
  (let ((rules-table (make-hash-table :test #'eq)))
    (mapc (lambda (rule)
            (setf (gethash (first rule) rules-table)
                  (adjoin (second rule) (gethash (first rule) rules-table))))
            rules)
    rules-table))

(defun make-orderings (orderings)
  (mapcar (s:curry #'mapcar #'parse-integer)
          (mapcar #'s:words orderings)))

(defun ordered-p (ordering sorter-fn)
  (equal ordering (sort (copy-seq ordering) sorter-fn)))

(defun seen-p (page seen rule-table)
  (let ((befores (gethash page rule-table)))
    (intersection seen befores)))

(defun sequential-p (ordering rules-table)
  (equal ordering
         (nreverse (reduce (lambda (seen page)
                             (unless (seen-p page seen rules-table)
                               (push page seen)))
                           ordering :initial-value nil))))

(defun middle-elt (list)
  (assert (oddp (length list)))
  (elt list (floor (length list) 2)))

(defun parsed-input (input)
  (multiple-value-bind (raw-rules raw-orderings) (split-rules input)
    (let* ((rules (parse-rules raw-rules))
           (rules-table (make-rules rules)))
      (values rules-table
              (make-orderings raw-orderings)))))

(defun challenge-1 (input)
  (multiple-value-bind (rules-table orderings) (parsed-input input)
    (reduce #'+
            (mapcar #'middle-elt
                    (s:filter (lambda (x) (sequential-p x rules-table))
                              orderings)))
    ;; (count-if #'middle-elt
    ;;           (s:filter (lambda (x) (ordered-p x rule-sorter)) orderings))

    ))

(defun toposorter (ordering rules)
  (let ((narrowed-rules
          (remove-if-not (lambda (rule) (member rule ordering))
                         rules :key #'car)))
    (s:toposort narrowed-rules)))

(defun order (ordering sorter-fn)
  (sort (copy-seq ordering) sorter-fn))

(defun challenge-2 (input)
  (multiple-value-bind (rules-table orderings) (parsed-input input)
    (multiple-value-bind (correct incorrect)
        (s:partition (lambda (x) (sequential-p x rules-table)) orderings)
      (declare (ignore correct))
      (let ((rules (parse-rules (split-rules input))))
        (reduce #'+
                (mapcar #'middle-elt
                        (mapcar (lambda (x)
                                  (order x (toposorter x rules)))
                                incorrect)))))))


;; (defun challenge-1 (input)
;;   (multiple-value-bind (raw-rules raw-orderings) (split-rules input)
;;     (let* ((rules (parse-rules raw-rules))
;;            ;; This would have been awesome if they rule graph wasn't inconsistent
;;            (rule-sorter (s:toposort rules))
;;            (orderings (make-orderings raw-orderings)))
;;       (count-if #'middle-elt
;;                 (s:filter (lambda (x) (ordered-p x rule-sorter)) orderings)))))



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
