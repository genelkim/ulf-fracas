
(in-package :ulf-fracas/tests)

(defun dumb-lenulf-fracas (str)
  "A Chinese room-style string to parse tree dictionary for FraCaS tests. These
  sentences were copied from lenulf:english-to-ulf outputs."
  (util:intern-symbols-recursive
    (cond
      ((equal str "An Italian became the world's greatest tenor.")
       '((AN.D~1 ITALIAN.N~2)
         ((PAST BECOME.V~3)
          ((THE.D~4 (N+PREDS WORLD.N~6 |'S~7|)) (GREATEST.A~8 TENOR.N~9)))
         |.|))

      ((equal str "There was an Italian who became the world's greatest tenor.")
       '(THERE.PRO~1
          (WAS.AUX~2
            ((AN.D~3 ITALIAN.A~4)
             (WHO.PRO~5
               ((PAST BECOME.V~6)
                ((THE.D~7 (N+PREDS WORLD.N~9 |'S~10|)) (GREATEST.A~11 TENOR.N~12))))))
          |.|))
      (t (error "Unknown sentence for dumb-parsing.")))
    :ulf-fracas))

(define-test run-fracas
  (:tag :fracas)
  (princ "Running a single fracas example.")
  (princln "NB: We are using saved raw ULF parses from Len's parser since the BLLIP parser is not accessible in Travis.")
  (princln " This will take several minutes...")
  (with-shadow (lenulf::english-to-ulf #'dumb-lenulf-fracas)
    ; verbose-fracas doesn't return anything
    (assert-false (ulf-fracas::verbose-fracas :steps 3 :ids '("001")))))

