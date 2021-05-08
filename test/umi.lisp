;; Junis Ekmekciu
;; Created on 09/24/2020
;; UMI test cases

(in-package :ulf-fracas/tests)

(setf *print-failures* t)
(setf *print-summary* t)

(defun test-umi-positive-polarity (arg1 arg2)
  (ulf-fracas:umi-positive-polarity arg1 arg2 :callpkg :ulf-fracas/tests))
(defun test-umi-negative-polarity (arg1 arg2)
  (ulf-fracas:umi-negative-polarity arg1 arg2 :callpkg :ulf-fracas/tests))

(define-test monotonicity-form
 ;; Tests for mon-form?
 (:tag :umi)
 (let ((c1 '((every.d a.n) ((pres be.v) (= (a.d b.n)))))
       (c2 '((every.d a.n) ((pres be.v) (= (an.d b.n)))))
       (n1 '((some.d a.n) ((pres be.v) (= (a.d b.n)))))
       (n2 '((a.d a.n) ((pres be.v) (= (a.d b.n)))))
       (n3 nil))
    (assert-true (mon-form? c1) c1)
    (assert-true (mon-form? c2) c2)
    (assert-false (mon-form? n1) n1)
    (assert-false (mon-form? n2) n2)
    (assert-false (mon-form? n3) n3)))

(define-test positive-umi
  (:tag :umi)
  ;; ((every.d dog.n) ((pres be.v) (= (an.d animal.n))))
  ;; (|Abelard| ((pres see.v) (= (a.d dog.n))))
  (assert-true
    (member '(|Abelard| ((pres see.v) (= (an.d animal.n))))
             (a2an (test-umi-positive-polarity '((every.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                          '(|Abelard| ((pres see.v) (= (a.d dog.n))))))
             :test #'equal))

  ;; ((the.d mother.n) ((past hug.v) (a.d (n+preds child.n (in_front_of.p (some.d (plur child.n)))))))
  ;; the mother hugged a child in front of some children
  ;; ((every.d child.n) ((pres be.v) (= (a.d human.n))))
  (assert-true
    (set-equal
      '(((the.d mother.n) ((past hug.v) (a.d (n+preds human.n (in_front_of.p (some.d (plur child.n)))))))
        ((the.d mother.n) ((past hug.v) (a.d (n+preds child.n (in_front_of.p (some.d (plur human.n))))))))
        ;((the.d mother.n) ((past hug.v) (a.d (n+preds human.n (in_front_of.p (some.d (plur human.n))))))))
      (test-umi-positive-polarity '((every.d child.n) ((pres be.v) (= (a.d human.n))))
                             '((the.d mother.n) ((past hug.v)
                                                 (a.d (n+preds child.n
                                                               (in_front_of.p (some.d (plur child.n))))))))
      :test #'equal))

  ;; ((plur cat.n) ((pres eat.v) (plur canary.n)))
  ;; ((every.d canary.n) ((pres be.v) (= (a.d bird.n))))
  (let ((expected '((k (plur cat.n)) ((pres eat.v) (k (plur bird.n)))))
        (arg1 '((every.d canary.n) ((pres be.v) (= (a.d bird.n)))))
        (arg2 '((k (plur cat.n)) ((pres eat.v) (k (plur canary.n))))))
    (assert-true
      (member expected (test-umi-positive-polarity arg1 arg2) :test #'equal)))

  ;; Must fail mon-form? function, with post-processing
  ;; ((every.d cat.n) ((pres be.v) (= (some.d animal.n))))
  ;; (they.pro ((pres own.v) (= (a.d cat.n))))
  (let ((fake-expected '(they.pro ((pres own.v) (= (an.d animal.n)))))
        (arg1 '((every.d cat.n) ((pres be.v) (= (some.d animal.n)))))
        (arg2 '(they.pro ((pres own.v) (= (a.d cat.n))))))
    (assert-false
      (member fake-expected (a2an (test-umi-positive-polarity arg1 arg2)) :test #'equal))))

(define-test negative-umi
  (:tag :umi)
  ;; Negative Polarity Tests
  ;; ((every.d dog.n) ((pres be.v) (= (an.d animal.n))))
  ;; (|Abelard| ((pres see.v) (= (a.d dog.n))))
  (assert-true
    (member '(|Abelard| ((pres see.v) (= (no.d dog.n))))
             (test-umi-negative-polarity '((every.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                    '(|Abelard| ((pres see.v) (= (no.d animal.n)))))
             :test #'equal))

  ;; NB(GK): This fails because of the polarity marking algorithm.
  (assert-true
    (set-equal
      '(((the.d mother.n) ((past hug.v) (no.d (n+preds child.n (in_front_of.p (some.d (plur child.n))))))))
      (test-umi-negative-polarity '((every.d child.n) ((pres be.v) (= (a.d human.n))))
                             '((the.d mother.n) ((past hug.v)
                                                 (no.d (n+preds human.n
                                                                (in_front_of.p (some.d (plur child.n))))))))
      :test #'equal))

  (assert-equal
    '(((the.d mother.n) ((past hug.v) (no.d (n+preds dog.n (in_front_of.p (k (plur child.n))))))))
    (test-umi-negative-polarity '((every.d dog.n) ((pres be.v) (= (an.d animal.n))))
                           '((the.d mother.n) ((past hug.v)
                                               (no.d (n+preds animal.n
                                                              (in_front_of.p (k (plur child.n)))))))))

  ;; ((no.d child.n) ((pres like.v) (k pepper.n)))
  ;; ((every.d child.n) ((pres be.v) (= (a.d human.n))))
  (assert-true
    (member '((no.d child.n) ((pres like.v) (k pepper.n)))
            (test-umi-negative-polarity '((every.d child.n) ((pres be.v) (= (a.d human.n))))
                                   '((no.d human.n) ((pres like.v) (k pepper.n))))
            :test #'equal))

  ;; Must fail mon-form? function, with post-processing
  ;; ((every.d sofa.n) ((pres be.v) (= (some.d furniture.n))))
  ;; (they.pro ((pres own.v) (= (no.d furniture.n))))
  (let ((fake-expected '(they.pro ((pres own.v) (= (no.d furniture.n)))))
        (arg1 '((every.d sofa.n) ((pres be.v) (= (some.d furniture.n)))))
        (arg2 '(they.pro ((pres own.v) (= (no.d sofa.n))))))
    (assert-false
     (member fake-expected (an2a (test-umi-negative-polarity arg1 arg2))
             :test #'equal)))

  ;; Must fail the negative polarity condition
  (let ((fake-expected '((the.d girl.n) ((past bring.v) (= (her.d toy.n)))))
        (arg1 '((every.d plushie.n) ((pres be.v) (= (a.d toy.n)))))
        (arg2 '((the.d girl.n) ((past bring.v) (= (her.d plushie.n))))))
    (assert-false
      (member fake-expected (test-umi-negative-polarity arg1 arg2)
              :test #'equal))))

(define-test umi-possessive
  (:tag :umi)
  ;; Must fail polarity form
  ;; ((the.d mother.n) ((past hug.v) (= (her.d child.n))))
  ;; ((every.d child.n) ((pres be.v) (= (a.d human.n))))
  (let ((fake-expected '((the.d mother.n) ((past hug.v) (= (her.d human.n)))))
        (arg1 '((every.d child.n) ((pres be.v) (= (a.d human.n)))))
        (arg2 '((the.d mother.n) ((past hug.v) (= (her.d child.n))))))
    (assert-false
      (member fake-expected (test-umi-positive-polarity arg1 arg2) :test #'equal))))

