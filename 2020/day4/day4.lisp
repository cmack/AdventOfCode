(ql:quickload "cl-ppcre")

(defun parse-stream (stream)
  (let ((buffer (make-array (file-length stream)
                            :element-type (stream-element-type stream))))
    (read-sequence buffer stream)
    (ppcre:split "\\s{2}"
                 (ppcre:regex-replace-all "\\n" buffer " "))))

(defun parse-input-file (filename)
  (with-open-file (in filename)
    (parse-stream in)))

(defparameter day4 (parse-input-file "../inputs/day4-input.txt"))

(defun tags (input)
  (let ((regex (ppcre:create-scanner
                "((?:byr|cid|e(?:cl|yr)|h(?:cl|gt)|iyr|pid)):(\\w+)")))
    (mapcar (lambda (line)
              (ppcre:all-matches-as-strings regex line))
            input)))

(defun valid (tags)
  (let ((diff (set-difference
               (list "byr:" "cid:" "hgt:" "hcl:" "eyr:" "pid:" "iyr:" "ecl:")
               tags
               :test #'string=)))
    (or (not diff)
        (equal diff (list "cid:")))))

(defun validate (input)
  (mapcar (lambda (tags)
  )
          input))
