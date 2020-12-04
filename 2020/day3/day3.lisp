(defun parse-stream (stream)
  (coerce (loop :for line = (read-line stream nil)
                :while line
                :collect line)
          'vector))

(defun parse-input-file (filename)
  (with-open-file (in filename)
    (parse-stream in)))

(defparameter day3 (parse-input-file "../inputs/day3-input.txt"))

(defun tree? (char)
  (char= char #\#))

(defun slope-count (input step-x step-y)
  (loop with len = (length (aref input 0))
        with limit = (length input)
        for i = 0 then (mod (+ i step-x) len)
        for j = 0 then (+ j step-y)
        while (< j limit)
        count (tree? (char (aref input j) i))))

;; star 1
(slope-count day3 3 1)

;;; star 2
(* (slope-count day3 1 1)
   (slope-count day3 3 1)
   (slope-count day3 5 1)
   (slope-count day3 7 1)
   (slope-count day3 1 2))
