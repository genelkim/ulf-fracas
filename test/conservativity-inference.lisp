
(in-package :ulf-fracas/tests)

(defun test-conservativity-inference (inulf)
  (util:inout-intern (inulf ulf :ulf-fracas :callpkg :ulf-fracas/tests)
    (ulf-fracas::conservativity-inference ulf)))

(define-test basic-conservativity
  (:tag :conservativity)
  
  (assert-equal '(((some.d man.n)
                   ((pres be.v)
                    (= (a.d (n+preds man.n
                                     (rel.rel ((pres become.v) famous.a))))))))
                (test-conservativity-inference
                  '((some.d man.n) ((pres become.v) famous.a)))))

