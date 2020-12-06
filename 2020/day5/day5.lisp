(ql:quickload "alexandria")

(defun binary-partition (list)
  (let ((half (/ (length list) 2)))
    (if (< half 1)
        list
        (cons (binary-partition (butlast list half))
              (binary-partition (nthcdr half list))))))

(defparameter *rows* (binary-partition (alexandria:iota 128)))
(defparameter *columns* (binary-partition (alexandria:iota 8)))

(defun b (row-list)
  (cdr row-list))

(defun f (row-list)
  (car row-list))

(defun r (column-list)
  (cdr column-list))

(defun l (column-list)
  (car column-list))

;; (defun seat-reducer (tree next)
;;   (destructuring-bind (row . column) tree
;;    (let ((fn (alexandria:symbolicate next)))
;;      (ecase fn
;;        ((F B) (cons (funcall fn row) column))
;;        ((R L) (cons row (funcall fn column)))))))


(defun seat-finder-reducer (finder next)
  (destructuring-bind (row . column)
      finder
    (let ((fn (alexandria:symbolicate next)))
      (ecase fn
        ((F B) (cons (alexandria:compose fn row) column))
        ((R L) (cons row (alexandria:compose fn column)))))))

;; (reduce #'seat-reducer "FBFBBFF" :start (list #'identity #'identity))

;; (defun seat-id (list)
;;   (+ (* 8 (caar list))
;;      (cadr list)))

;; (defun seat-from-string (string)
;;   (seat-id (reduce #'seat-reducer string
;;                    :initial-value (cons *rows* *columns*))))

(defun seat-finder (string)
  (reduce #'seat-finder-reducer string
          :initial-value (cons #'identity #'identity)))

(defun seat-finder-id (finders)
  (+ (* 8 (first (funcall (first finders) *rows*)))
     (first (funcall (rest finders) *columns*))))

(defun seat-ids ()
  (with-open-file (in "../inputs/day5-input.txt")
    (loop for line = (read-line in nil)
          while line
          collect (seat-finder-id (seat-finder line)))))

;;; Day 5 part 1
(reduce #'max (seat-ids))               ; 890

;;; Day5 part 2
(let* ((seat-ids (sort (seat-ids) #'<)))
  (set-difference (alexandria:iota (+ 1 (length seat-ids))
                                   :start (first seat-ids))
                  seat-ids))
