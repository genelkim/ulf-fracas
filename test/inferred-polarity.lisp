
(in-package :ulf-fracas/tests)

(defun ulf2slf (inulf scoping)
  "Converts to slf via interning in ulf-fracas."
  (inout-intern (inulf ulf :ulf-fracas :callpkg :ulf-fracas/tests)
    (let ((lulf (ulf-fracas::dfs-label-tree ulf))
          lslf)
      (setf lslf (ulf-fracas::lulf2lslf lulf scoping))
      (ulf-fracas::unlabeled-tree lslf))))

(defun num-atoms (ulf)
  (cond
    ((atom ulf) 1)
    (t
     (reduce #'+
             (mapcar #'num-atoms ulf)))))

(define-test test-lulf2lslf
  (:tag :lulf2lslf)


	(let (ulf lulf lslf slf scoping)
    
    ;; Example 1
    (setf ulf
          '((every.d (n+preds person.n
                              (who.rel ((pres have.v)
                                        (the.d (n+preds right.n
                                                        (= (to (live.v (in.p | EUROPE|))))))))))
            ((pres be.v)
             (= (a.d (n+preds person.n
                              (who.rel ((pres have.v)
                                        (the.d (n+preds right.n
                                                        (= (to (live.v (in.p | EUROPE|))))))))
                              (who.rel ((pres can.aux-v)
                                        (travel.v freely.adv-a
                                                  (within.p | EUROPE|))))))))))
    (setf scoping '(((6 13)) ((0 33 2) (((((37 44)))))) ((0 2 33) (((((37 44))))))))
    (setf slf (ulf2slf ulf scoping))
    (assert-true (> (num-atoms slf) (num-atoms ulf))
                 slf
                 ulf)))

;lulf: #<LABELED-TREE [0 ([1 ([2 EVERY.D] [3 ([4 N+PREDS] [5 PERSON.N] [6 ([7 WHO.REL] [8 ([9 ([10 PRES] [11 HAVE.V])] [12 ([13 THE.D] [14 ([15 N+PREDS] [16 RIGHT.N] [17 ([18 =] [19 ([20 TO] [21 ([22 LIVE.V] [23 ([24 IN.P] [25 | EUROPE|])])])])])])])])])])] [26 ([27 ([28 PRES] [29 BE.V])] [30 ([31 =] [32 ([33 A.D] [34 ([35 N+PREDS] [36 PERSON.N] [37 ([38 WHO.REL] [39 ([40 ([41 PRES] [42 HAVE.V])] [43 ([44 THE.D] [45 ([46 N+PREDS] [47 RIGHT.N] [48 ([49 =] [50 ([51 TO] [52 ([53 LIVE.V] [54 ([55 IN.P] [56 | EUROPE|])])])])])])])])] [57 ([58 WHO.REL] [59 ([60 ([61 PRES] [62 CAN.AUX-V])] [63 ([64 TRAVEL.V] [65 FREELY.ADV-A] [66 ([67 WITHIN.P] [68 | EUROPE|])])])])])])])])])]>
;lslf: #<LABELED-TREE [0 ([1 #:G2256] [26 ([27 ([28 PRES] [29 BE.V])] [30 ([31 =] [32 #:G2258])])])]>
;Unknown polarity for scoping operator: #:G2256
;
;
;lulf: #<LABELED-TREE [0 ([1 ([2 EVERY.D] [3 ([4 N+PREDS] [5 PERSON.N] [6 ([7 WHO.REL] [8 ([9 ([10 PRES] [11 HAVE.V])] [12 ([13 THE.D] [14 ([15 N+PREDS] [16 RIGHT.N] [17 ([18 =] [19 ([20 TO] [21 ([22 LIVE.V] [23 ([24 IN.P] [25 | EUROPE|])])])])])])])])])])] [26 ([27 ([28 PRES] [29 BE.V])] [30 ([31 =] [32 ([33 A.D] [34 ([35 N+PREDS] [36 PERSON.N] [37 ([38 WHO.REL] [39 ([40 ([41 PRES] [42 HAVE.V])] [43 ([44 THE.D] [45 ([46 N+PREDS] [47 RIGHT.N] [48 ([49 =] [50 ([51 TO] [52 ([53 LIVE.V] [54 ([55 IN.P] [56 | EUROPE|])])])])])])])])] [57 ([58 WHO.REL] [59 ([60 ([61 PRES] [62 CAN.AUX-V])] [63 ([64 TRAVEL.V] [65 FREELY.ADV-A] [66 ([67 WITHIN.P] [68 | EUROPE|])])])])])])])])])]>
;lslf: #<LABELED-TREE [0 ([1 #:G2675] [26 ([27 ([28 PRES] [29 BE.V])] [30 ([31 =] [32 #:G2677])])])]>
;Unknown polarity for scoping operator: #:G2675

