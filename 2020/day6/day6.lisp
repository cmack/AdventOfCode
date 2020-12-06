(ql:quickload "split-sequence")

(defparameter day6 (with-open-file (in "../inputs/day6-input.txt")
                     (loop for line = (read-line in nil)
                           while line
                           collect line)))

(defmacro string-cat (&rest sequences)
  `(concatenate 'string ,@sequences))

(defun challenge1 (input)
  (let ((delimited (split-sequence:split-sequence "" input :test #'string=)))
    (mapcar (lambda (answers)
              (remove-duplicates (reduce (lambda (current next)
                                           (string-cat current next))
                                         answers)
                                 :test #'char=))
            delimited)))

(defun challenge2 (input)
  (let ((delimited (split-sequence:split-sequence "" input :test #'string=)))
    (mapcar (lambda (answers)
              (remove-duplicates
               (reduce (lambda (current next)
                         (intersection current (coerce next 'list)))
                       answers
                       :initial-value (coerce "abcdefghijklmnopqrstuvwxyz" 'list))
               :test #'char=))
            delimited)))

;;; star 1
(reduce #'+ (mapcar #'length (challenge1 day6)))

;;; star 2
(reduce #'+ (mapcar #'length (challenge2 day6)))
