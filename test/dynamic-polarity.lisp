
(in-package :ulf-fracas/tests)

;; Custom testing external versions of internal functions.
(defun annotate-polarity (inx)
  (inout-intern (inx x :ulf-fracas :callpkg :ulf-fracas/tests)
    (ulf-fracas::annotate-polarity x)))

(defun get-segment-polarity (x y z)
  (let ((primitive (ulf-fracas::get-segment-polarity x y z)))
    (if (null primitive)
      primitive
      (intern (symbol-name primitive) :ulf-fracas/tests))))

(defun run-natlog (x)
  (intern-symbols-recursive (ulf-fracas::run-natlog x) :ulf-fracas/tests))

(define-test dp-various-structures
  (:tag :dynamic-polarity)
  ;; Tests for various syntactic structures that have special handling in
  ;; dynamic polarity.
  ;; Passives are transformed
  (assert-true
    (util:tree-find (annotate-polarity '(i.pro ((past (pasv give.v)) (a.d book.n))))
                    'given.v))
  ;; Plur operators are retained.
  (assert-true
    (util:tree-find (annotate-polarity '(i.pro ((past pet.v) (some.d (plur dog.n)))))
                    'dog.n)))

(define-test test-get-segment-polarity
  (:tag :dynamic-polarity)
  ;; This function is internal an depends on the exact symbol and cons
  ;; references. Therefore, rather than interning, but values will directly be
  ;; defined within the :ulf-fracas package.
  (let* ((ulfpart1 (list '(ulf-fracas::PRES ulf-fracas::HAVE.V)
                         '(ulf-fracas::K (ulf-fracas::PLUR ulf-fracas::TAIL.N)))) ;VP
         (ulfpart2 'ulf-fracas::CAT.N) ;N
         (ulfpart2-par (list 'ulf-fracas::plur ulfpart2)) ;plur N
         (ulfpart3 (list 'ulf-fracas::all.d ulfpart2-par)) ;det + N
         (compulf (list ulfpart3 ulfpart1)) ;Formula
         (diffulf '((ulf-fracas::ALL.D (ulf-fracas::PLUR ulf-fracas::CAT.N))
                    ((ulf-fracas::PRES ulf-fracas::HAVE.V)
                     (ulf-fracas::K (ulf-fracas::PLUR ulf-fracas::TAIL.N))))))
    (assert-equal '+ (get-segment-polarity ulfpart1 compulf compulf)
                  ulfpart1 compulf)
    (assert-equal nil (get-segment-polarity ulfpart1 compulf diffulf)
                  ulfpart1 compulf diffulf)
    (assert-equal NIL (get-segment-polarity ulfpart1 diffulf compulf)
                  ulfpart1 diffulf compulf)
    (assert-equal NIL (get-segment-polarity ulfpart1 diffulf diffulf)
                  ulfpart1 diffulf)
    (assert-equal '- (get-segment-polarity ulfpart2 ulfpart2-par compulf)
                  ulfpart2 ulfpart2-par compulf)
    (assert-equal NIL (get-segment-polarity ulfpart2 ulfpart2-par diffulf)
                  ulfpart2 ulfpart2-par diffulf)
    (assert-equal NIL (get-segment-polarity ulfpart2 nil compulf)
                  ulfpart2 compulf)))

(define-test ulf-through-natlog
  (:tag :dynamic-polarity)
  (assert-equal '((("Mary" +) ("knows" +) ("that" +) ("John" +) ("went" +) ("home" +)))
                (run-natlog (list (ulf-fracas::ulf-to-string
                                    '(|Mary| ((PRES KNOW.V) (THAT (|John| ((PAST GO.V) (K HOME.N))))))))))

  (assert-equal '((("all" +) ("cats" -) ("have" +) ("tails" +)))
                (run-natlog (list (ulf-fracas::ulf-to-string
                                    '((ALL.D (PLUR CAT.N)) ((PRES HAVE.V) (K (PLUR TAIL.N)))))))))

