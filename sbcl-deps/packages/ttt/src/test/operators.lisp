
(in-package :ttt/tests)

(define-test vertical
  (:tag :operator :vertical)
  ;; Example from paper.
  ;; (ˆ@ (+ (@ *)) X) -- matches any tree with leftmost leaf X
  (let ((pattern '(^@ (+ (@ _*)) X)))
    (assert-true (ttt:match-expr pattern '(X)))
    (assert-true (ttt:match-expr pattern '(X Y Z)))
    (assert-true (ttt:match-expr pattern '((X Y) Z)))
    (assert-true (ttt:match-expr pattern '(((((((((X) Y))))))) Z)))
    (assert-false (ttt:match-expr pattern '(Y)))
    (assert-false (ttt:match-expr pattern '(A (Y X))))
    (assert-false (ttt:match-expr pattern '((Y X) Z)))
    (assert-false (ttt:match-expr pattern '(A (B (C (D ((Y X))) Z))))))

  (let ((pattern '(^@ _! (C _*) E)))
    (assert-true (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C D E))))
    (assert-false (ttt:match-expr pattern '(A B (C D) F)))
    (assert-false (ttt:match-expr pattern '(A B (D E) F)))
    (assert-false (ttt:match-expr pattern '(C D E))))

  (let ((pattern '(^@ _! (<> (C D E)) E)))
    (assert-true (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C D E))))
    (assert-false (ttt:match-expr pattern '(A B (D C E))))
    (assert-false (ttt:match-expr pattern '(A B (C E) F)))
    (assert-false (ttt:match-expr pattern '(A B (C D) F)))
    (assert-false (ttt:match-expr pattern '(A B (D E) F)))
    (assert-false (ttt:match-expr pattern '(C D E))))

  (let ((pattern '(^@ _! (({} C D E)) E)))
    (assert-true (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(B (C D E) F)))
    (assert-true (ttt:match-expr pattern '(A B (C D E))))
    (assert-true (ttt:match-expr pattern '(A B (D C E))))
    (assert-false (ttt:match-expr pattern '(A B (C E) F)))
    (assert-false (ttt:match-expr pattern '(A B (C D) F)))
    (assert-false (ttt:match-expr pattern '(A B (D E) F)))
    (assert-false (ttt:match-expr pattern '(C D E))))

  (let ((pattern '(^@ ({} (A _*) (_* B)) E)))
    (assert-true (ttt:match-expr pattern '(A (C E B))))
    (assert-true (ttt:match-expr pattern '((A E C) B)))
    (assert-false (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(A B (C D E))))
    (assert-false (ttt:match-expr pattern '(A B (D C E)))))
  
  (let ((pattern '(^@ ({} (A _*) (_* B)) E)))
    (assert-true (ttt:match-expr pattern '(A (C E B))))
    (assert-true (ttt:match-expr pattern '((A E C) B)))
    (assert-false (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(A B (C D E))))
    (assert-false (ttt:match-expr pattern '(A B (D C E)))))
  
  (let ((pattern '(^@ (<> (A _*) (_* B)) E)))
    (assert-true (ttt:match-expr pattern '(A (C E B))))
    (assert-false (ttt:match-expr pattern '((A E C) B)))
    (assert-false (ttt:match-expr pattern '(A B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(B (C D E) F)))
    (assert-false (ttt:match-expr pattern '(A B (C D E))))
    (assert-false (ttt:match-expr pattern '(A B (D C E)))))

  (let ((pattern '(^@ _! A)))
    (assert-false (ttt:match-expr pattern 'A))
    (assert-true (ttt:match-expr pattern '(A)))
    (assert-false (ttt:match-expr pattern '((A)))))
  (let ((pattern '(^@ _* A)))
    (assert-true (ttt:match-expr pattern 'A))
    (assert-true (ttt:match-expr pattern '(A)))
    (assert-true (ttt:match-expr pattern '((A))))
    (assert-false (ttt:match-expr pattern '((B)))))
  (let ((pattern '(^@ _+ A)))
    (assert-false (ttt:match-expr pattern 'A))
    (assert-true (ttt:match-expr pattern '(A)))
    (assert-true (ttt:match-expr pattern '((A))))
    (assert-false (ttt:match-expr pattern '((B)))))
  (let ((pattern '(^@ _? A)))
    (assert-true (ttt:match-expr pattern 'A))
    (assert-true (ttt:match-expr pattern '(A)))
    (assert-false (ttt:match-expr pattern '((A))))
    (assert-false (ttt:match-expr pattern 'B)))
  )
    



