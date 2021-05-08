
(in-package :ulf-fracas/tests)

;; Test package versions of the functions.
(defun fix-ulf (inulf)
  (inout-intern (inulf ulf :standardize-ulf :callpkg :ulf-fracas/tests)
    (standardize-ulf::standardize-ulf ulf)))
(defun lift-not (inulf)
  (inout-intern (inulf ulf :ulf-fracas :callpkg :ulf-fracas/tests)
    (ulf-fracas::lift-not ulf)))

;; NB: The ULF parser can only be run on the cs.rochester.edu cluster right now
;; since it uses the BLLIP parser installed there. We don't have a way to
;; install this in arbitrary locations right now. So use a lookup table instead
;; of precomputed formulas for the tests.
(defun dumb-lenulf-parse-ulf (str)
  "A Chinese room-style string to parse tree dictionary for a sample of FraCaS
  examples. These sentences were copied from lenulf:english-to-ulf outputs."
  (cond
    ((equal str "No really great tenors are modest.")
     '(((NO.D~1 ((REALLY.ADV~2 GREAT.A~3) (PLUR~4 TENOR.N~5)))
        (ARE.AUX~6 MODEST.A~7) |.|~8)))
    ((equal str "An Italian became the world's greatest tenor.")
     '(((AN.D~1 ITALIAN.N~2)
       ((PAST~3 BECOME.V~4)
        ((THE.D~5 (N+PREDS~6 WORLD.N~7 |'S|~8)) (GREATEST.A~9 TENOR.N~10)))
       |.|~11)))
    ((equal str "Several great tenors are British.")
     '(((K~1 (SEVERAL.A~2 (GREAT.A~3 (PLUR~4 TENOR.N~5))))
        (ARE.AUX~6 (PLUR~7 BRITISH~8)) |.|~9)))
    (t (error "Unknown sentence for dumb-parsing."))))

(defun parse-ulf (str)
  (intern-symbols-recursive (ulf-fracas::parse-ulf str) :ulf-fracas/tests))

;;
;; Tests.
;;

(define-test not-fix
  (:tag :parse-ulf)
  (assert-equal '(i.pro ((past do.aux-s) not go.v))
                (fix-ulf '(i.pro ((past do.aux-s) not.adv go.v))))

  (assert-equal '(i.pro ((past do.aux-s) not go.v))
                (fix-ulf '(i.pro ((past do.aux-s) |N'T.ADV| go.v)))))


(define-test lift-not
  (:tag :parse-ulf)
  (assert-equal '(not (i.pro ((past do.aux-s) go.v)))
                (lift-not '(not i.pro ((past do.aux-s) go.v))))
  (assert-equal '(not ((every.d person.n) (pres know.v)))
                (lift-not '(not (every.d person.n) (pres know.v))))
  (assert-equal '(he.pro ((pres know.v)
                          (that (not (i.pro ((pres be.v) green.a))))))
                (lift-not '(he.pro ((pres know.v)
                                    (that (i.pro ((pres be.v) not green.a)))))))
  (assert-equal '(i.pro ((pres know.v) (to (not sleep.v))))
                (lift-not '(i.pro ((pres know.v) (to (not sleep.v))))))
  (assert-equal '(the.d (n+preds man.n
                                 (sub tht.rel
                                      (not (i.pro ((pres do.aux-s) (know.v *h)))))))
                (lift-not '(the.d (n+preds man.n
                                           (sub tht.rel
                                                (i.pro ((pres do.aux-s) not
                                                                        (know.v *h)))))))))


(define-test parse-ulf
  (:tag :parse-ulf)
  ;; Define a locally scoped version of parse-ulf and test the post-processing.
  (with-shadow (lenulf::english-to-ulf #'dumb-lenulf-parse-ulf)
    (assert-equal '((no.d ((really.mod-a great.a) (plur tenor.n)))
                    ((pres be.v) modest.a))
                  (parse-ulf "No really great tenors are modest."))

    (assert-equal '((an.d |Italian|.n)
                    ((past become.v)
                     (((the.d world.n) 's) (greatest.a tenor.n))))
                  (parse-ulf "An Italian became the world's greatest tenor."))

    (assert-equal '((several.d (great.a (plur tenor.n)))
                    ((pres be.v) |British|.a))
                  (parse-ulf "Several great tenors are British."))))

;; Removes the period from the ULF
(defparameter *ttt-period-fix*
  (list
     standardize-ulf::*ttt-remove-periods*
     standardize-ulf::*ttt-remove-parenthesis*))

(defun test-remove-period (in-ulf)
   (inout-intern (in-ulf ulf :standardize-ulf :callpkg :ulf-fracas/tests)
     (ttt:apply-rules *ttt-period-fix* ulf :deepest t)))

;; Test cases for removing periods and double parenthesis from ULFs.
(define-test remove-perpar
    (:tag :parse-ulf)
    (assert-equal '((every.d carp.n) ((pres be.v) (= (a.d fish.n))))
                   (test-remove-period '(((every.d carp.n) ((pres be.v) (= (a.d fish.n)))) \.)))

    (assert-equal '((the.d prisoner.n) ((pres may.aux-s) escape.v))
                   (test-remove-period '(((\. the.d prisoner.n) ((pres \. may.aux-s) escape.v)) \.)))

    (assert-equal '(|John| (((past see.v) |Mary|) twice.adv-f))
                   (test-remove-period '((|John| \. (((\. past \. see.v \.) \. |Mary|) twice.adv-f)) \.) ))

    (assert-equal '((|Angela| ((pres perf) leave.v)) ?)
                   (test-remove-period '(((\. |Angela| (\. (pres perf) \. leave.v)) \. ?))))

    (assert-equal '((\" (k Love.n) \") ((pres be.v) (a.d ((four.a letter.n) word.n))))
                   (test-remove-period '(((\" (k Love.n \.) \") ((\. pres \. be.v) (a.d ((four.a \. letter.n) word.n)
                                                                               \.) \.) \.) \.)))
    (assert-equal '((|Angela| ((pres perf) leave.v)) ?)
                   (test-remove-period '((((\. |Angela| ((\. (pres perf) \. leave.v))) \. ?)))))

    (assert-equal '((|Angela| ((pres perf) leave.v)) ?)
                   (test-remove-period '((((\. |Angela| (\. ((pres perf)) \. leave.v)) \. ?))))))


