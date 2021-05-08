
(in-package :ulf-fracas/tests)

(setf *print-failures* t)
(setf *print-summary* t)

(define-test conversion-form
  "Tests for is-conversion-form"
  (:tag :uci)
  (let ((c1 '((some.d a.n) ((pres be.v) (= (a.d b.n)))))
        (c2 '((an.d a.n) ((pres be.v) (= (a.d b.n)))))
        (c3 '((some.d dog.n) ((pres be.v) (= (an.d animal.n)))))
        (n1 '((every.d a.n) ((pres be.v) (= (a.d b.n)))))
        (n2 nil))
    (assert-true (is-conversion-form c1) c1)
    (assert-true (is-conversion-form c2) c2)
    (assert-true (is-conversion-form c3) c3)
    (assert-false (is-conversion-form n1) n1)
    (assert-false (is-conversion-form n2) n2)))

(define-test dummy-uci  ; test name
  "Dummy tests for UCI" ; test description
  (:tag :uci)           ; tags for grouping tests (:tag is not one of them, that's indicating it's a tag list)
  ; (<test> <expected> <value> {Optional <print value1> <print value2> ...}) 
  (assert-equal '((some.d animal.n) ((pres be.v) (= (an.d dog.n))))
                (conversion-rule '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                 :callpkg :ulf-fracas/tests))

  ; With post-processing
  (assert-equal '((some.d animal.n) ((pres be.v) (= (a.d dog.n))))
                (an2a (conversion-rule '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                       :callpkg :ulf-fracas/tests)))

  ; With let, allows us to print the variales on failure.
  (let ((expected '((some.d animal.n) ((pres be.v) (= (a.d dog.n)))))
        (input '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))))
    (assert-equal expected (an2a (conversion-rule input :callpkg :ulf-fracas/tests)) 
                  input))

  ; Counter example.
  (let ((fake-expected '((some.d person.n) ((pres be.v) (= (every.d salesman.n)))))
        (input '((some.d salesman.n) ((pres be.v) (= (every.d person.n))))))
    (assert-false (equal fake-expected (conversion-rule input :callpkg :ulf-fracas/tests))
                         fake-expected input)))

(define-test dummy-uci-ttt  ; test name
  "Dummy tests for UCI with TTT" ; test description
  (:tag :uci)           ; tags for grouping tests (:tag is not one of them, that's indicating it's a tag list)
  ; (<test> <expected> <value> {Optional <print value1> <print value2> ...}) 
  (assert-equal '((some.d animal.n) ((pres be.v) (= (an.d dog.n))))
                (conversion-rule-ttt '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                     :callpkg :ulf-fracas/tests))

  ; With post-processing
  (assert-equal '((some.d animal.n) ((pres be.v) (= (a.d dog.n))))
                (an2a (conversion-rule-ttt '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))
                                           :callpkg :ulf-fracas/tests)))

  ; With let, allows us to print the variales on failure.
  (let ((expected '((some.d animal.n) ((pres be.v) (= (a.d dog.n)))))
        (input '((some.d dog.n) ((pres be.v) (= (an.d animal.n))))))
    (assert-equal expected (an2a (conversion-rule-ttt input :callpkg :ulf-fracas/tests))
                  input))

  ; Counter example.
  (let ((fake-expected '((some.d person.n) ((pres be.v) (= (every.d salesman.n)))))
        (input '((some.d salesman.n) ((pres be.v) (= (every.d person.n))))))
    (assert-false (equal fake-expected (conversion-rule-ttt input :callpkg :ulf-fracas/tests))
                         fake-expected input)))

