;; Gene Kim 7-27-2018
;; Basic lisp-unit example.  Some errors have been introduced on purpose to 
;; show how failures look in lisp-unit.

;; Load in "init-tests.lisp" and try the following commands.
;; (run-tests) ; runs all tests (this will include the real tests)
;; (run-tests '(dummy-fn-basic-test dummy-fn-list-test dummy-fn-edge-case-test)) ; runs the selected subset of tests (by name)
;; (run-tags '(:example)) ; runs selected subset of tests (by tag)
;; (run-tags '(:dummy-fn))
;;
;; Then try to fix some of the failures and run again to see if it worked! (remember to reload the file).

(in-package :ulf-fracas/tests)

(setq *print-failures* t) ; prints info on failed tests
(setq *print-summary* t) ; prints summary of all tests.

;; Expected behavior: 
;;  Recursively replace all strings with "dummy"
(defun dummy-fn (x)
  (cond ((stringp x) "dummy")
        ;; Uncomment below to eliminate test failures.
        ;((listp x) (mapcar #'dummy-fn x))
        (t x)))

(define-test dummy-fn-basic-test
  "Testing basic functionality of dummy-fn"
  (:tag :example :dummy-fn :basic)
  (assert-equal "dummy" (dummy-fn "hi"))
  (assert-equal "dummy" (dummy-fn "LLONG STRIIIIIIIIIIIINNNNNNNNNGGGGGG!!!!!!!!!!!!    YOLOLOLOLOLOOLO")))

(define-test dummy-fn-list-test
  "Testing recursive functionality of dummy-fn"
  (:tag :example :dummy-fn :recursive)
  (assert-equal '("dummy" "dummy" "dummy") (dummy-fn '("x" "y" "z")))
  (assert-equal '(((("dummy")))) (dummy-fn '(((("str")))))))

(define-test dummy-fn-edge-case-test
  "Testing edge cases and bad inputs on dummy-fn"
  (:tag :example :dummy-fn :bad-input)
  (assert-equal nil (dummy-fn nil))
  (assert-eql nil (dummy-fn nil))
  (assert-equal 'x (dummy-fn 'x))
  (assert-equal -1 (dummy-fn -1)))

;; You can supply addition arguments to assert-equal which the system, which
;; lisp-unit will print out the values of upon failure.  This is useful for
;; examples where the values themselves are not enough to debug the code.
(define-test extra-vals-example
  "Example of supplying additional arguments to assert-equal debugability"
  (:tag :example)
  (let ((lst1 '(1 2 3))
        (lst2 '(a b c))
        temp1 temp2)
    ;; Doin' some stuff n' things 
    ;; Making it hard to tell what's going on.
    (setq temp1 (append (cdr lst1)
                        (list (car lst1) (car lst2) (cddr lst2))))
    (setq temp2 (cons (subseq lst1 0 2) (subseq lst2 1 (length lst2))))
    
    (assert-equal (length temp1) (length temp2) ; if this fails the numbers aren't gonna be helpful.
                  temp1 temp2 lst1 lst2))) ; so print out some extra info.


;; list-assert-equal is a function I defined in test/test-util.lisp.
;; It uses lisp-unit assert-equal to check that the expected and actual lists
;; are of equal length and that each argument is the same (in order).
(define-test list-assert-equal-example
  "Example of a test using list-assert-equal"
  (:tag :example :list-assert-equal)
  (let ((lst1 '(1 2 3))
        (lst2 '(a b c)))
    (list-assert-equal '(1 2 3) lst1)
    (list-assert-equal '(a b c) lst2)
    (list-assert-equal '(a b c) (cdr lst1)) ; expected to introduce failure
    (list-assert-equal '(1 2) '(1 b)) ; expected to introduce failure
    (list-assert-equal '("works on strings too!") '("works on strings too!"))
    (list-assert-equal '("but the strings need to be the same") '("but the stringzzz need to be the same")) ; expected to introduce failure
    ;; Works recursively because 'equal is defined recursively.
    (list-assert-equal '((a b) c) '((a b) c))
    (list-assert-equal '((a b) c) '((a c) c)) ; expected to introduce failure
    (list-assert-equal '((This.pro ((pres be.v) (a.d (|ULF| formula.n)))))
                       '((This.pro ((pres be.v) (a.d (|ULF| formula.n))))))))






