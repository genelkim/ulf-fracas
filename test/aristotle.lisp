
(in-package :ulf-fracas/tests)

(defun dumb-lenulf-aristotle (str)
  "A Chinese room-style string to parse tree dictionary for Aristotelian
  inferences. These sentences were copied from lenulf:english-to-ulf outputs."
  (cond
    ((equal str "Every M is a P")
     '((EVERY.D~1 M.N~2) (IS.AUX~3 (A.D~4 P.N~5))))
    ((equal str "Every S is a M")
     '((EVERY.D~1 S.N~2) (IS.AUX~3 (A.D~4 M.N~5))))
    ((equal str "Every S is a P")
     '((EVERY.D~1 S.N~2) (IS.AUX~3 (A.D~4 P.N~5))))
    ((equal str "Some N is a M")
     '((SOME.D~1 M.N~2) (IS.AUX~3 (A.D~4 N.N~5))))
    ((equal str "Some N is a P")
     '((SOME.D~1 N.N~2) (IS.AUX~3 (A.D~4 P.N~5))))
    ((equal str "No M is a P")
     '((NO.D~1 M.N~2) (IS.AUX~3 (A.D~4 P.N~5))))
    ((equal str "Every N is a M")
     '((EVERY.D~1 N.N~2) (IS.AUX~3 (A.D~4 M.N~5))))
    ((equal str "No N is a P")
     '((NO.D~1 N.N~2) (IS.AUX~3 (A.D~4 P.N~5))))
    ((equal str "Not every N is a P")
     '(NOT.ADV~1 (EVERY.D~2 N.N~3) (IS.AUX~4 (A.D~5 P.N~6))))
    (t (error "Unknown sentence for dumb-parsing."))))

(define-test symbolic-aristotle
  (:tag :aristotle)
  ;; Super simple test which runs the symbolic aristotelian syllogims and
  ;; checks that all four are correct. See #'run-ulf-aristotelian-syllogism in
  ;; evaluation/aristotle.lisp for details.
  ;;
  ;; NB: This test will run longer than most unit tests since it's running four
  ;; complete inference tasks, including some premise/hypothesis shuffling.
  (princ "Running all four symbolic Aristotelian syllogism inference tasks.")
  (princln " This will take several minutes...")
  (assert-equal 4 (ulf-fracas::run-ulf-aristotelian-syllogism :steps 3 :verbosity 0))

  ; Check verbose version, but for fewer steps.
  (assert-equal 3 (ulf-fracas::run-ulf-aristotelian-syllogism :steps 1 :verbosity 2)))

(define-test english-aristotle
  (:tag :aristotle)
  ;; Same as symbolic-aristotle but starting with English sentences and parsing
  ;; them with lenulf + some postprocessing.
  (princ "Running all four English Aristotelian syllogism inference tasks.")
  (princln " This will take several minutes...")
  ;(princln "NB: This will cause execution errors on Travis since it doesn't have access to the BLLIP parser.")
  (princln "NB: We are using saved raw ULF parses from Len's parser since the BLLIP parser is not accessible in Travis.")
  (princln " This will take several minutes...")
  (with-shadow (lenulf::english-to-ulf #'dumb-lenulf-aristotle)
    (assert-equal 4 (ulf-fracas::run-string-aristotelian-syllogism :steps 3
                                                                 :verbosity 0))
    ; Check verbose version, but for fewer steps.
    (assert-equal 3 (ulf-fracas::run-string-aristotelian-syllogism :steps 1
                                                                 :verbosity 2))))

