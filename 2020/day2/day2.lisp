(ql:quickload "cl-ppcre")               ; not ready to go through trouble of
                                        ; setting up a project.

(defun parse-stream (stream)
  (let ((regex (ppcre:create-scanner "(?:^(\\d+)-(\\d+)\\s(\\w):\\s(\\w+)$)")))
    (loop :for line = (read-line stream nil)
          :while line
          :collect (ppcre:register-groups-bind
                       ((#'parse-integer number-1 number-2) letter string)
                       (regex line)
                     (list number-1 number-2 (char letter 0) string)))))

(defun parse-input-file (filename)
  (with-open-file (in filename)
    (parse-stream in)))

(defun parse-input-string (string)
  (with-input-from-string (in string)
    (parse-stream in)))

(defun valid-p (min max char string)
  (<= min (count char string) max))

(defun valid-p-2 (pos1 pos2 char string)
  (flet ((position-char= (position char)
           (char= char (char string (1- position)))))
   (let ((match-1 (position-char= pos1 char))
         (match-2 (position-char= pos2 char)))
     (and (or match-1 match-2)
          (not (and match-1 match-2))))))

(defun valid-counts (input validator-fn)
  (loop for parsed-line in input
        count (apply validator-fn parsed-line)))

(defun valid-counts-2 (input validator-fn)
  (count-if (lambda (x) (apply validator-fn x)) input))

;;; Star 1
(valid-counts (parse-input-file "../inputs/day2-input.txt") #'valid-p)

;;; Star 2
(valid-counts (parse-input-file "../inputs/day2-input.txt") #'valid-p-2)
