;;;; chapter 2: making recommendations
(in-package :cl-pci)

(defvar *critics* nil)
(when (null *critics*)
    (chapter2-init))

(defmacro assoc-string (item alist &rest rest)
  `(assoc ,item ,alist :test #'equal ,@rest))

(defun critics (critic-ht)
  (let ((critic-lst nil))
    (maphash (lambda (k v) (declare (ignore v)) (push k critic-lst)) critic-ht)
    critic-lst))

(defun get-critic (critic critic-lst)
  (gethash critic critic-lst))

(defun get-score (movie critic critic-lst)
  (let ((movie-score (assoc-string movie
                                   (get-critic critic critic-lst))))
    (if (null movie-score)
        nil
        (cdr movie-score))))

(defun shared-movies (critic-lst p1 p2)
  (mapcar #'car (intersection (get-critic p1 critic-lst)
                              (get-critic p2 critic-lst)
                              :key #'car :test #'equal)))

(defun sim-distance (critic-lst p1 p2)
  (labels ((diffscore (movie)
             (expt (- (get-score movie p1 critic-lst)
                      (get-score movie p2 critic-lst))
                   2)))
    (/ 1 (+ 1
            (reduce #'+
                    (mapcar #'diffscore
                            (shared-movies critic-lst p1 p2)))))))

(defun sum-movie-scores (movies person critic-lst)
  (reduce #'+
          (mapcar (lambda (movie)
                    (get-score movie person critic-lst))
                  movies)))

(defun sum-square-movie-scores (movies person critic-lst)
  (reduce #'+
          (mapcar (lambda (movie)
                    (expt (get-score movie person critic-lst) 2))
                  movies)))

(defun sim-pearson (critic-lst p1 p2)
  (let ((shared (shared-movies critic-lst p1 p2)))
    (if (zerop (length shared))
        0
        (let* ((sum1 (sum-movie-scores shared p1 critic-lst))
               (sum2 (sum-movie-scores shared p2 critic-lst))
               (sum1sq (sum-square-movie-scores shared p1 critic-lst))
               (sum2sq (sum-square-movie-scores shared p2 critic-lst))
               (n (length shared))
               (p-sum
                  (reduce #'+
                          (mapcar (lambda (movie)
                                    (* (get-score movie p1 critic-lst)
                                       (get-score movie p2 critic-lst)))
                                  shared)))
               (num (- p-sum
                       (/ (* sum1 sum2) n)))
               (den (sqrt (* (- sum1sq
                                (/ (expt sum1 2) n))
                             (- sum2sq
                                (/ (expt sum2 2) n))))))
          (if (zerop den) 0
              (/ num den))))))


(defun other-scores (critic-lst person &key (sim #'sim-pearson))
  (let ((others (filter (lambda (x) (not (equal x person)))
                        (critics critic-lst)))
        (results nil))
    (mapc (lambda (critic)
              (push
               (cons critic
                     (funcall sim critic-lst person critic))
               results))
          others)
    results))

(defun top-matches (critic-lst person &key (n 5) (sim #'sim-pearson))
  (let ((match-list (sort (other-scores critic-lst person :sim sim)
                          #'> :key #'cdr))
        (matches nil))
    (dotimes (i n)
      (push (nth i match-list) matches))
    (reverse matches)))


(defun not-seen (critic-lst person others)
  (let ((seen (mapcar #'car (get-critic person critic-lst)))
        (unseen '()))
    (mapcar (lambda (critic)
              (mapcar (lambda (movie)
                        (let ((title (car movie)))
                          (pushnew title unseen :test #'equal)))
                      (get-critic critic critic-lst)))
            others)
    (filter (lambda (o) (zerop (count o seen :test #'equal))) unseen)))

(defun movie-similarity (critic-lst others title)
  (reduce #'+
          (mapcar (lambda (critic)
                    (* (cdr critic)
                       (let ((score (get-score title (car critic) critic-lst)))
                         (if (null score) 0 score))))
                  others)))

(defun sim-scores (critic-lst titles others)
  (mapcar (lambda (title)
            (cons title
                  (movie-similarity critic-lst others title)))
          titles))

(defun sim-totals (critic-lst titles others)
  (let ((totals nil))
    (dolist (title titles)
      (let ((score (reduce #'+
                           (mapcar #'cdr
                                   (filter (lambda (critic)
                                             (get-score title
                                                        (car critic)
                                                        critic-lst))
                                           others)))))
        (push (cons title score) totals)))
    totals))

(defun recommendations (critic-lst person &key (sim #'sim-pearson))
  (let* ((others (filter (lambda (o) (>(cdr o) 0))
                         (other-scores critic-lst person :sim sim)))
         (titles (not-seen critic-lst
                           person (mapcar #'car others)))
         (scores (sim-scores critic-lst titles others))
         (totals (sim-totals critic-lst titles others)))
    (sort (mapcar (lambda (title)
                    (cons title
                          (/ (cdr (assoc title scores))
                             (cdr (assoc title totals)))))
                  titles)
          #'> :key #'cdr)))
