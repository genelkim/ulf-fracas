;; Implements various heuristic functions.

(in-package :ulf-fracas)

(defun get-ordered-leaves (ulf)
  (cond
    ((atom ulf) (list ulf))
    (t (apply #'append
              (mapcar #'get-ordered-leaves ulf)))))

(defun get-sorted-leaves (ulf)
  (let ((leaves (get-ordered-leaves ulf)))
    (sort leaves #'string<)))

(defun unordered-leaf-f1 (ulf1 ulf2)
  "Computes leaf f1 score ignoring order.

  2 * (# of leaf overlap) / (# ulf1 leaves + # ulf2 leaves)

  don't allow one leaf to match to multiple in the other formula.
  Invert this (1 - value) to make it so it's a cost."
  (let* ((leaves1 (get-sorted-leaves ulf1))
         (leaves2 (get-sorted-leaves ulf2))
         (len1 (length leaves1))
         (len2 (length leaves2))
         overlap)
    (loop while (and (not (null leaves1))
                     (not (null leaves2)))
          do
          (cond
            ((eql (car leaves1) (car leaves2))
             (push (car leaves1) overlap)
             (setf leaves1 (cdr leaves1))
             (setf leaves2 (cdr leaves2)))
            ((string< (car leaves1) (car leaves2))
             (setf leaves1 (cdr leaves1)))
            (t
             (setf leaves2 (cdr leaves2)))))
    (- 1
       (/ (* 2 (length overlap))
          (+ len1 len2)))))

(defun unordered-leaf-f1-new-only-heuristic (new goal kb)
  "Computes the minimum leaf f1 between the new formulas and the goal."
  (let ((f1s (fset:image #'(lambda (f) (unordered-leaf-f1 f goal))
                         new)))
    (fset:reduce #'min f1s)))

