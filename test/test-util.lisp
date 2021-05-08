;;; Gene Kim 7-27-2018
;;; Utility functions for testing.

(in-package :ulf-fracas/tests)

(defun an2a (ulf)
  (cond
    ((and (atom ulf) (eql 'an.d ulf)) 'a.d)
    ((atom ulf) ulf)
    (t (mapcar #'an2a ulf))))

(defun a2an (ulf)
  (cond
    ((and (atom ulf) (eql 'a.d ulf)) 'an.d)
    ((atom ulf) ulf)
    (t (mapcar #'a2an ulf))))

;; Helper function to assert equality between lists, one by one.
(defun list-assert-equal (actual expect)
  (assert-equal (length expect) (length actual)
                (length expect) (length actual)
                expect actual)
  (mapcar #'(lambda (x)
              (let ((expect (first x))
                    (actual (second x)))
                (assert-equal expect actual
                              expect actual)))
          (mapcar #'list expect actual)))

;; Helper function to assert that two lists are equal, ignoring order and
;; repetitions.
(defun set-assert-equal (actual expect)
  (let ((unique-actual (remove-duplicates actual :test #'equal))
        (unique-expect (remove-duplicates expect :test #'equal))
        shared expect-only actual-only
        reordered-expect reordered-actual)
    ;; Construct a reordered version of each list such that elements that
    ;; are shared come first in both.
    (setq shared (intersection unique-expect unique-actual :test #'equal))
    (setq expect-only (set-difference unique-expect unique-actual
                                      :test #'equal))
    (setq actual-only (set-difference unique-actual unique-expect
                                      :test #'equal))
    (setq reordered-expect (append shared expect-only))
    (setq reordered-actual (append shared actual-only))
    ;; Assert equal length.
    (assert-equal (length reordered-expect) (length reordered-actual)
                  (length reordered-expect) (length reordered-actual)
                  reordered-expect reordered-actual)
    ;; Assert that each element at a time.
    (mapcar #'(lambda (x)
                (let ((expect-elem (first x))
                      (actual-elem (second x)))
                  (assert-equal expect-elem actual-elem
                                expect-elem actual-elem
                                reordered-expect reordered-actual)))
            (mapcar #'list reordered-expect reordered-actual))))

;; From Doug Hoyte's Let Over Lambda.
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Make the functions available at compile time, for macros.
  ;; https://stackoverflow.com/a/49922152
	(defun flatten (x)
	  (labels ((rec (x acc)
	             (cond ((null x) acc)
	                   ((atom x) (cons x acc))
	                   (t (rec
	                        (car x)
	                        (rec (cdr x) acc))))))
	    (rec x nil)))

	(defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "G!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "O!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "G!"
          (subseq (symbol-name s) 2))))


(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
                (remove-if-not #'g!-symbol-p
                               (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
               (lambda (s)
                 `(,s (gensym ,(subseq
                                 (symbol-name s)
                                 2))))
               syms)
         ,@body))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))


;; From Stack Overflow: https://stackoverflow.com/a/9037946
(defmacro! with-shadow ((fname fun) &body body)
  "Shadow the function named fname with fun
   Any call to fname within body will use fun, instead of the default function for fname.
   This macro is intentionally unhygienic:
   fun-orig is the anaphor, and can be used in body to access the shadowed function"
  `(let ((fun-orig))
     (cond ((fboundp ',fname)
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (setf (symbol-function ',fname) fun-orig)))
           (t
            (setf (symbol-function ',fname) ,fun)
            (unwind-protect (progn ,@body)
              (fmakunbound ',fname))))))

