(uiop:define-package #:cmack.advent-of-code.2024.day9
    (:use :cl)
  (:local-nicknames (:s :serapeum/bundle)))

(in-package #:cmack.advent-of-code.2024.day9)

(defun numchar->num (numchar)
  (- (char-int numchar) #.(char-int #\0)))

(defun parse-lines (input)
  (mapcar (lambda (l)
            (mapcar #'numchar->num (coerce l 'list)))
          input))

(defun test (input)
  (first (parse-lines (with-input-from-string (in input)
                        (loop for line = (read-line in nil)
                              while line
                              collect line)))))

(defun day9 ()
  (first (parse-lines (uiop:read-file-lines "day9-input.txt"))))

(defun expand (input)
  (let ((expansion
          (loop
            for (file space) on input by #'cddr
            for id from 0
            nconc (nconc (make-list file :initial-element id)
                         (when space (make-list space :initial-element nil))))))
    (coerce expansion 'vector)))

(defun compact (input)
  (loop for i below (length input)
        for f = (svref input i)
        for end-file = (position-if #'numberp input :from-end t)
        until (= i end-file)
        when (null f)
          do (rotatef (svref input i) (svref input end-file))
        finally (return input)))

(defun checksum (compacted)
  (let ((disk (substitute 0 nil compacted)))
    (reduce #'+ (map 'vector #'* disk (s:iota (length disk))))))


(defparameter *test* "2333133121414131402")


(defun part1 (input)
  (checksum (compact (expand input))))

(defun empty-space-p (size)
  (lambda (space)
    (>= (count nil space) size)))

(defun file-space-p ()
  (lambda (space)
    (some #'numberp space)))

(defun empty-space-pos (n)
  (lambda ()
    (search)))

(defun fit-file (seq file start)
  (let* ((write-start (position nil (elt seq start)))
         (write-end (+ write-start (length file))))
    (s:splice-seqf (elt seq start) :new file
                                   :start write-start
                                   :end write-end))
  seq)

(defun collapse-vector (seq)
  (reduce (s:curry #'concatenate 'vector) seq))

(defun nullify-vector (seq)
  (coerce (make-list (length seq)) 'vector))

;;; New thought:
;;; have another expanded. delete nil reverse and get runs
;;; iterate over main and s:splice in

;;; expanded has all the nils; chunked does not and is reversed

(defun compact2 (expanded)
  ;; let* ((expanded (s:runs expanded-input)))
  (loop for chunk-end = (position-if #'numberp expanded :from-end t)
          then (position-if #'numberp expanded :from-end t :end (1+ chunk-start))
        for chunk-val = (elt expanded chunk-end)
        for chunk-start = (position-if (lambda (x) (not (eql x chunk-val))) expanded
                                       :from-end t :end chunk-end)
        while (and chunk-start (> chunk-start 0))
        do (let* ((chunk-size (- chunk-end chunk-start))
                  (empty-fits (search (make-list chunk-size) expanded :end2 (1+ chunk-start))))
             (when (and chunk-start empty-fits)
               (incf chunk-start)
               (incf chunk-end)
               ;; (format t "chunk: ~a end: ~a val: ~a empty-fits: ~a~%~s~%"
               ;;         chunk-start chunk-end chunk-val empty-fits expanded)
               (replace expanded expanded :start1 empty-fits :end1 (+ empty-fits chunk-size)
                                          :start2 chunk-start :end2 chunk-end)
               (replace expanded (make-list chunk-size) :start1 chunk-start :end1 chunk-end)))
        finally (return expanded)))

(defun part2 (input)
  (checksum (compact2 (expand input))))

;;; 6227018846942 too high
;;; 6227018762750
