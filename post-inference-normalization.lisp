
(in-package :ulf-fracas)

(defun flatten-n+preds (ulf)
  (ttt:apply-rules
    (list
      ;; Remove nested n+preds
      '(/ (n+preds _*1 (n+preds _*2) _*3)
          (n+preds _*1 _*2 _*3))
      ;; Remove repeated elements (not strictly true because of the effect of
      ;; repetition in language.
      '(/ (n+preds _*1 _!. _!. _*2)
          (n+preds _*1 _!. _*2)))
    ulf :rule-order :slow-forward))

(defun collapse-bleached-nominals-in-copulas (ulf)
  "Collapse bleached nominals that are introduced in the monotonic inferences
  of non-nominal predicates if these are indefinite arguments to a copula, thus
  not necessary to be nouns anymore. Example:
    every dog is green, Spot is a dog
    -> Spot is a thing that is green
    -[collapse]-> Spot is green
  Note in non-copular cases this is not possible, e.g.
    every dog is green, I saw a dog
    -> I saw a thing that is green
    *-[collapse]-> I saw green
  is not a reasonable inference (though in the case of colors specifically this
  turns out to be true."
  (ttt:apply-rule
    '(/ (((!1 lex-tense?) be.v) (= ((! a.d an.d k) (n+preds thing.n (that.rel ((lex-tense? be.v) pred?))))))
        ((!1 be.v) pred?))
    ulf))

(defun remove-redundant-reification (ulf)
  "Some inference rules introduce reification of predicates for fluency:
  (= (k ...)). This is sometimes redundant and needs to be removed."
  (ttt:apply-rule
    '(/ (= (k (= _!)))
        (= _!))
    ulf))

