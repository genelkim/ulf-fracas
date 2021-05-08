
(in-package :ulf-fracas/tests)

(defun test-flatten-n+preds (inulf)
  (util:inout-intern (inulf ulf :ulf-fracas :callpkg :ulf-fracas/tests)
    (ulf-fracas::flatten-n+preds ulf)))

(define-test flatten-n+preds
  (:tag :post-inf)
  (assert-equal
    '(i.pro ((past see.v) (the.d (n+preds man.n sleeve.n (of.p mine.pro)))))
    (test-flatten-n+preds
      '(i.pro ((past see.v) (the.d (n+preds man.n (n+preds sleeve.n (of.p mine.pro))))))))

  (assert-equal
    '(i.pro ((pres see.v) (the.d (n+preds man.n (with.p (k hair.n))))))
    (test-flatten-n+preds
      '(i.pro ((pres see.v) (the.d (n+preds man.n man.n (with.p (k hair.n)))))))))

