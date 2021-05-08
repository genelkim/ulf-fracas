
(in-package :ulf-fracas/tests)

(defun test-quantifier-polarity (ulf)
  (ulf-fracas:quantifier-polarity ulf :callpkg :ulf-fracas/tests))

(define-test basic-quantifier-subst
  (:tag :quantifier)

  ;; Positive direction.
  (assert-true
    (set-equal
      '((i.pro ((past see.v) (a.d dog.n))))
      (test-quantifier-polarity
        '(i.pro ((past see.v) (the.d dog.n))))
      :test #'equal))
      
  ;; Negative direction.
  (assert-true
    (set-equal
      '((i.pro ((past do.aux-s) not (see.v (the.d dog.n)))))
      (test-quantifier-polarity
        '(i.pro ((past do.aux-s) not (see.v (a.d dog.n)))))
      :test #'equal)))

