;; Mandar Juvekar 10-1-2020
;; Inference engine for ULF natural logic inferences

(in-package :ulf-fracas/tests)

(defun timestwo (x) (list (* x 2)))    ; unary rule
(defun subtract (x y) (list (- x y)))  ; binary rule

(define-test forward-inf-integer-test
  (:tag :nlog-inference-engine :nlog-forward-inf)
  (let ((ulf-fracas::*unary-rules* '(timestwo)))
    (assert-true (set-equal (infer-nlog '(5 11) 10 :callpkg :ulf-fracas/tests)
                            '(5632 2560 1408 640 352 160 88 40 22 10 5 11 20 44 80 176
                              320 704 1280 2816 5120 11264))))
  (let ((ulf-fracas::*binary-rules* '(subtract)))
    (assert-true (set-equal (infer-nlog '(5 11) 2 :callpkg :ulf-fracas/tests)
                            '(-5 -11 -12 -17 12 1 -1 17 0 -6 6 5 11))))
  (let ((ulf-fracas::*unary-rules* '(timestwo)) (ulf-fracas::*binary-rules* '(subtract)))
    (assert-true (set-equal (infer-nlog '(5 11) 2 :callpkg :ulf-fracas/tests)
                            '(-5 -22 -17 -16 1 17 -1 16 28 4 -4 -28 -10 -11 -6 6 11 5
                              10 22 -12 20 44 12 0)))))

(define-test goal-directed-contradiction
  (:tag :nlog-inference-engine :nlog-goal-directed)
  (let ((ulf-fracas::*unary-rules*
          (list 'ulf-fracas::conservativity-inference
                'ulf-fracas::equivalent-quantifier-substitution-inferences))
         (premises
           '(((NO.D (GREAT.A (PLUR TENOR.N))) ((PRES BE.V) MODEST.A))))
         (goal
           '(THERE.PRO ((PRES BE.V) (= (K (N+PREDS (GREAT.A (PLUR TENOR.N))
                                                   (WHO.REL ((PRES BE.V) MODEST.A))))))))
         (*package* (find-package :ulf-fracas))) ; for readability.
    (assert-equal :c
                  (ulf-fracas::goal-directed-infer-nlog
                    premises
                    goal
                    :max-steps 1
                    :track-sources nil
                    :callpkg :ulf-fracas/tests))))

(define-test goal-directed-entailment
  (:tag :nlog-inference-engine :nlog-goal-directed)
  (let ((ulf-fracas::*unary-rules*
          (list 'ulf-fracas::conservativity-inference
                'ulf-fracas::equivalent-quantifier-substitution-inferences))
         (premises
           '(((ALL.D (GREAT.A (PLUR TENOR.N))) ((PRES BE.V) MODEST.A))))
         (goal
           '(THERE.PRO ((PRES BE.V) (= (K (N+PREDS (GREAT.A (PLUR TENOR.N))
                                                   (WHO.REL ((PRES BE.V) MODEST.A))))))))
         (*package* (find-package :ulf-fracas))) ; for readability.
    (assert-equal :e
                  (ulf-fracas::goal-directed-infer-nlog
                    premises
                    goal
                    :max-steps 1
                    :track-sources nil
                    :callpkg :ulf-fracas/tests))))

(define-test goal-directed-unknown
  (:tag :nlog-inference-engine :nlog-goal-directed)
  (let ((ulf-fracas::*unary-rules*
          (list 'ulf-fracas::conservativity-inference
                'ulf-fracas::equivalent-quantifier-substitution-inferences))
         (premises
           '(((ALL.D (GREAT.A (PLUR TENOR.N))) ((PRES BE.V) MODEST.A))))
         (goal
           '((ALL.D (HAPPY.A (PLUR TENOR.N))) ((PRES BE.V) MODEST.A)))
         (*package* (find-package :ulf-fracas))) ; for readability.
    (assert-equal :u
                  (ulf-fracas::goal-directed-infer-nlog
                    premises
                    goal
                    :max-steps 1
                    :track-sources nil
                    :callpkg :ulf-fracas/tests))))

