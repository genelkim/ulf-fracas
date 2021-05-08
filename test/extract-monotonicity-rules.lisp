
(in-package :ulf-fracas/tests)

(defun test-extract-monotonicity-rules (ulf)
  (ulf-fracas:extract-monotonicity-rules ulf :callpkg :ulf-fracas/tests))

(define-test extract-monotonicity-from-n+preds
  (:tag :extract-rules)

  (assert-true
    (set-equal
      '(((every.d (n+preds book.n (for.p (ka write.v))))
         ((pres be.v) (= (a.d book.n))))
        ((every.d (n+preds book.n (for.p (ka write.v))))
         ((pres be.v) (for.p (ka write.v)))))
      (test-extract-monotonicity-rules
        '(i.pro ((past buy.v) (a.d (n+preds book.n (for.p (ka write.v)))))))
      :test #'equal)))

