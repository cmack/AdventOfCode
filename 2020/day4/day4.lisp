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

;;; star 2
(defun valid-byr? (string)
  (multiple-value-bind (year digits)
      (parse-integer string)
    (and (= 4 digits)
         (<= 1920 year 2002))))

(defun valid-iyr? (string)
  (multiple-value-bind (year digits)
      (parse-integer string)
    (and (= 4 digits)
         (<= 2010 year 2020))))

(defun valid-eyr? (string)
  (multiple-value-bind (year digits)
      (parse-integer string)
    (and (= 4 digits)
         (<= 2020 year 2030))))

(defun valid-hgt? (string)
  (let ((height (parse-integer string :junk-allowed t)))
    (or (and (equal 3 (search "cm" string))
             (<= 150 height 193))
        (and (equal 2 (search "in" string))
             (<= 59 height 76)))))

(defun valid-hcl? (string)
  (and (char= #\# (char string 0))
       (multiple-value-bind (number digits)
           (parse-integer string :start 1 :radix 16)
         (declare (ignore number))
         (= 7 digits))))

(defun valid-ecl? (string)
  (member string (list "amb" "blu" "brn" "gry" "grn" "hzl" "oth")
          :test #'string=))

(defun valid-pid? (string)
  (and (= 9 (length string))
       (every #'digit-char-p string)))

(defun valid-tag? (input)
  (let ((list-tag (ppcre:split ":" input)))
    (destructuring-bind (header value) list-tag
      (funcall (alexandria:eswitch (header :test #'string=)
                 ("byr" #'valid-byr?)
                 ("iyr" #'valid-iyr?)
                 ("eyr" #'valid-eyr?)
                 ("hgt" #'valid-hgt?)
                 ("hcl" #'valid-hcl?)
                 ("ecl" #'valid-ecl?)
                 ("pid" #'valid-pid?)
                 ("cid" #'identity))
               value))))

(defun same-tag? (string1 string2)
  (string= (subseq string1 0 3)
           (subseq string2 0 3)))

(defun required-tag? (string)
  (member string
          (list "byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid" "cid")
          :test #'same-tag?))

(defun required-tags-present? (list)
  (and (= (length (remove "cid" list :test #'same-tag?)) 7)
       (every #'required-tag? list)))

(defun star2 (input)
  (let ((tags (mapcar (lambda (x) (ppcre:split "\\s" x)) input)))
    (remove-if-not (lambda (tag-list)
                     (every #'valid-tag? tag-list))
                   (remove-if-not #'required-tags-present? tags))))

(length (star2 day4))
