;;; Gene Kim 7-23-2018
;;;
;;; Code for high-level inference management.
;;; This is loaded last because it refers to functions defined in all the other
;;; components.

(in-package :ulf-fracas)

;; Creating the d1 and d2 labels for the conversion rule
(defparameter *d1* (list 'some.d 'a.d 'an.d 'no.d))
(defparameter *d2* (list 'some.d 'a.d 'an.d))

;; Function that checks if the list given is in the form of the upper
;; part of the conversion formula. The form:
;;    ((*d1* <pred>) ((<tense> be.v) (= (*d2* <pred>))))
(defun is-conversion-form (inulf)
  (util:in-intern (inulf ulf :ulf-fracas)
    (and
      (listp ulf)
      (= (length ulf) 2)
      ; (*d1* <pred>)
      (listp (first ulf))
      (= (length (first ulf)) 2)
      (member (first (first ulf)) *d1*)
      (ulf-lib:pred? (second (first ulf)))
      ; ((<tense> be.v) (= (*d2* <pred>)))
      (listp (second ulf))
      (= (length (second ulf)) 2)
      (let ((tbe (first (second ulf)))
            (eqd2pred (second (second ulf))))
        (and
          ; (<tense> be.v)
          (listp tbe)
          (= (length tbe) 2)
          (ulf-lib:lex-tense? (first tbe))
          (equal (second tbe) 'be.v)
          ; (= (*d2* <pred>))
          (listp eqd2pred)
          (= (length eqd2pred) 2)
          (equal (first eqd2pred) '=)
          ; (*d2* <pred>)
          (listp (second eqd2pred))
          (= (length (second eqd2pred)) 2)
          (member (first (second eqd2pred)) *d2*)
          (ulf-lib:pred? (second (second eqd2pred))))))))

;; Conversion rule that returns the lower part of the formula and
;; takes as an input the upper part of the formula
(defun conversion-rule (inulf &key (callpkg nil))
  (util:inout-intern (inulf ulf :ulf-fracas :callpkg callpkg)
    (if (is-conversion-form ulf)
      ; We've checked the form so we can expand it safely.
      (let* ((d1 (first (first ulf)))
             (p1 (second (first ulf)))
             (vp (second ulf)) ; ((<tense> be.v) (= (*d2* pred2)))
             (tense (first (first vp)))
             (d2 (first (second (second vp))))
             (p2 (second (second (second vp)))))
        ; Rebuild, replacing the two predicates.
        (list (list d1 p2)
              (list (list tense 'be.v)
                    (list '= (list d2 p1))))))))
(util:memoize 'conversion-rule)

(defun uci-list (ulf)
  (let ((res (conversion-rule-ttt ulf)))
    (if (and (not (null res))
             (not (equal res ulf)))
      (list res)
      nil)))

(defun uci-list-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from uci-list.

  Finds the two points where the result and source diverge and just swaps the
  polarities of those two subtrees. Since the conversion rule only works when
  both the subject and object of the copula have indefinites or no.d scoping,
  which have symmetric polarities on the restrictor and body, we can do this
  without keeping track of the scoping relations.

  Arguments:
    ulf: The resulting ULF
    src-ulfs: List of ULFs (in this case with one element) that led to ULF.
  Returns:
    A polarity marked version of `ulf` based on the sources."
  (assert (= 1 (length src-ulfs)))
  (labels
    ((apply-polarity-with-divergence-map (curmlulf
                                          srcmlulf
                                          divergence-map
                                          topsrcml)
       "Applies the polarity from a source taking account subtree movement
       through a divergence map. The divergence map gives the corresponding
       index position of the second tree at points of divergence. At these
       points, the current second tree is replaced with point marked in the
       divergence map.

       Arguments:
         curmlulf: The multi-labeled-tree-node for the current ULF.
         srcmlulf: The multi-labeled-tree-node for the source ULF, including
           polarity.
         divergence-map: A list of two elements mapping the curmlulf index to
           the corresponding srcmlulf index when the trees diverge.
         topsrcml: The multi-labeled-tree for the top of the source ULF (for
           accessing subtrees at divergences)
       Returns:
         A labeled-tree with the polarity markings of curmlulf."
       (let ((cur-srcmlulf srcmlulf)
             (curexpr (expr curmlulf))
             curpol)
         ;; At a divergence, replace srcmlulf using the divergence map.
         (when (assoc (index curmlulf) divergence-map)
           (setf cur-srcmlulf
                 (node-at-index
                   topsrcml
                   (cdr (assoc (index curmlulf) divergence-map)))))
         (setf curpol (get-label-from-key
                        topsrcml
                        'polarity
                        (index cur-srcmlulf)))
         (cond
           ;; Base case.
           ((atom (expr curmlulf))
            (make-instance 'labeled-tree :expr curexpr :label curpol))
           ;; Recursive case.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (curchild srcchild)
                                (apply-polarity-with-divergence-map
                                  curchild srcchild divergence-map topsrcml))
                            (expr curmlulf)
                            (expr cur-srcmlulf))
              :label curpol)))))) ; end of labels defs

    ;; Main body of labels.
    (let* ((src-mulf (get-infer-nlog-marked-ulf (first src-ulfs)))
           (reslulf (dfs-label-tree ulf))
           (srclulf (dfs-label-tree (first src-ulfs)))
           (divergences (find-divergences reslulf srclulf))
           ;; Build the multi-label trees.
           (resmlulf (build-multi-labeled-tree reslulf nil nil))
           (srcmlulf (build-multi-labeled-tree
                       srclulf
                       (list src-mulf)
                       '(polarity)))
           divergence-map)
      (assert (= 2 (length divergences)))
      ;; Swap the indices of the divergences.
      (setf divergence-map
            (list (cons (car (first divergences))
                        (cdr (second divergences)))
                  (cons (car (second divergences))
                        (cdr (first divergences)))))
      (labeled-tree-to-marked-ulf
        (apply-polarity-with-divergence-map
          (root resmlulf)
          (root srcmlulf)
          divergence-map
          srcmlulf)))))
;; Register this function for marking dispatch.
(setf (gethash 'uci-list *infer-nlog-marking-dispatch-fn-ht*)
      #'uci-list-marking-from-source-dispatch)

(defun conversion-rule-ttt (inulf &key (callpkg nil))
  (util:inout-intern (inulf ulf :ulf-fracas :callpkg callpkg)
    (ttt:apply-rules '((/ (((!1 some.d a.d an.d no.d) (!2 pred?))   ; matches (d1 pred1)
                           (((!3 lex-tense?) be.v)                  ; matches (tense be.v)
                            (= ((!4 some.d a.d an.d) (!5 pred?))))) ; matches (= (d2 pred2))

                        ; rebuild swapping pred1 (!2) and pred2 (!5)
                        ((!1 !5) ((!3 be.v) (= (!4 !2))))))
                    ulf)))
(util:memoize 'conversion-rule-ttt)

;; Matches a monotonicity rule ULF
;; ((every.d/all.d P1) ((pres be.v) (= (indef.d/k P2))))
;; or more generally
;; ((every.d/all.d P1) ((pres be.v) <pred>))
(defun mon-form? (inulf)
  (in-intern (inulf ulf :ulf-fracas)
    (ttt:match-expr '(((! every.d all.d) pred?)
                      ((pres be.v)
                       ; this is redundant
                       (! (= ((! a.d an.d k) pred?))
                          pred?)))
                    ulf)))
;; Extracts the first predicate from a monotonicity ULF.
(defun get-mon-p1 (ulf)
  (second (first ulf)))
;; Extracts the second predicate from a monotonicity ULF.
;; If it is a noun (= (indef/k <pred>)), simply pull it out.
;; If it's not a noun, turn it into one with (n+preds thing.n (that.rel ((pres be.v) <pred>)))
(defun get-mon-p2 (ulf)
  (cond
    ;; Simple noun case.
    ((ttt:match-expr '(_! ; subj
                       (_! ; copula
                        (= ((! a.d an.d k some.d) pred?))))
                     ulf)
     (second (second (second (second ulf)))))
    ;; Simple term case.
    ((ttt:match-expr '(_! (_! (= term?))) ulf)
     (second (second ulf)))
    ;; General case.
    (t (let ((rawpred (second (second ulf))))
         (list 'n+preds 'thing.n (list 'that.rel (list '(pres be.v) rawpred)))))))

;; Helper function that checks the polarity of a predicate and
;; its determiner, switches p1 with p2
;; and adds the change to the list of ULFs returned
;;
;; Arguments:
;;  curpred  - predicate to replace
;;  newpred  - the new value for the replaced predicate
;;  b        - marked ULF
;;  bu       - unmarked ULF
;;  pol      - polarity of the predicate context
(defun tr-switch-pred (curpred newpred b bu pol)
  ;; The base case is of the form:
  ;; (pred1 pol)
  (cond
    ((and (ulf-lib:pred? bu) (equal bu curpred) (equal (global-polarity b) pol))
     ;; replace curpred with newpred. No need to recurse since curpred won't
     ;; match any subcomponent of bu.
     (list newpred))
    ;; Special case: the cur and new preds are both equality with a term and we
    ;; found a matching term. Ignore the equal signs and just replace the terms.
    ((and (listp curpred) (= 2 (length curpred)) (eql '= (first curpred))
          (listp newpred) (= 2 (length newpred)) (eql '= (first newpred))
          (equal (second curpred) bu)
          (equal (global-polarity b) pol))
     (list (second newpred)))
    ((listp bu)
     (apply #'append
            (mapcar (lambda (i)
                      (index-recurse i curpred newpred (get-ulf b) bu pol))
                    (range (length bu)))))
    (t nil)))

(defun range (max &key (min 0) (step 1))
    (loop for n from min below max by step collect n))

;; Recurses into the current index and returns the value with the results of
;; the recursion inserted into the correct position of the input formula.
(defun index-recurse (i cp np b bu pol)
    (let* ((c (nth i b))
           (cu (nth i bu))
           (recval (tr-switch-pred cp np c cu pol))
           (pre-slice (subseq bu 0 i))
           (post-slice (subseq bu (1+ i))))
        (mapcar (lambda (x) (append pre-slice (list x) post-slice)) recval)))

;; Monotonicity rule that returns the lower part of the formula and
;; takes as an input the upper parts of the formula
;; {in}definite-det? --> use to check delta
(defun umi-positive-polarity (inulf1 inulf2 &key (callpkg nil))
  (inout-intern (inulf1 ulf1 :ulf-fracas :callpkg callpkg)
    (in-intern (inulf2 ulf2 :ulf-fracas)
      (let (results)
        (if (and (mon-form? ulf1) (listp ulf2))
          (setf results
                (append results
                        (tr-switch-pred (get-mon-p1 ulf1)
                                        (get-mon-p2 ulf1)
                                        (get-infer-nlog-marked-ulf ulf2)
                                        ulf2
                                        '+))))

        (if (and (mon-form? ulf2) (listp ulf1))
          (setf results
                (append results
                        (tr-switch-pred (get-mon-p1 ulf2)
                                        (get-mon-p2 ulf2)
                                        (get-infer-nlog-marked-ulf ulf1)
                                        ulf1
                                        '+))))
        results))))

(defun umi-negative-polarity (inulf1 inulf2 &key (callpkg nil))
  (inout-intern (inulf1 ulf1 :ulf-fracas :callpkg callpkg)
    (in-intern (inulf2 ulf2 :ulf-fracas)
      (let (results)
        (if (and (mon-form? ulf1) (listp ulf2))
          (setf results
                (append results
                        (tr-switch-pred (get-mon-p2 ulf1)
                                        (get-mon-p1 ulf1)
                                        (get-infer-nlog-marked-ulf ulf2)
                                        ulf2
                                        '-))))
        (if (and (mon-form? ulf2) (listp ulf1))
          (setf results
                (append results
                        (tr-switch-pred (get-mon-p2 ulf2)
                                        (get-mon-p1 ulf2)
                                        (get-infer-nlog-marked-ulf ulf1)
                                        ulf1
                                        '-))))
        results))))

;;; Dispatch Fn
;;; 1. Get divergences between result and the two inputs, the one with fewer divergences is the basis function, the other is the monotonicity rule.
;;; 2. Get the polarity marking of the basis
;;; 3. Get the polarity marking of the corresponding predicate of the monotonicity rule.
;;; 4. Use the polarity marking of the basis, until the divergence. At the divergence, use the predicate polarity. No polarity flipping is necessary since negative contexts lead to the antecedent which is also in negative context adn vice versa.
(defun umi-marking-dispatch-fn (ulf src-ulfs)
  "Dispatch polarity marking function for UMI rules."
  (assert (= 2 (length src-ulfs)))
  (labels
    ((apply-polarity-with-divergences (reslulf basis-mulf basis-divergence-point mon-mulf in-divergence)
       "Given divergence point and current divergence state transfers polarity
       from the source ulfs to the result ulf."
       (let ((in-divergence (or in-divergence
                                (= (label reslulf) basis-divergence-point))))
         (cond
           ;; In divergence and match with monotonicity predicate, use the monotonicity predicate.
           ((and in-divergence
                 (equal (unlabeled-tree reslulf) (unlabeled-tree mon-mulf)))
            mon-mulf)
           ;; In divergence and atom.
           ((and in-divergence (atom (expr reslulf)))
            (make-instance
              'labeled-tree
              :expr (expr reslulf)
              :label (label mon-mulf)))
           ;; Otherwise in divergence, recurse and label current monotonicity with top of predicate.
           (in-divergence
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (reschild)
                                (apply-polarity-with-divergences
                                  reschild
                                  nil ; not used in divergence
                                  basis-divergence-point
                                  mon-mulf
                                  in-divergence))
                            (expr reslulf))
              :label (label mon-mulf)))
           ;; Base case.
           ((atom (expr reslulf))
            (make-instance 'labeled-tree
                           :expr (expr reslulf)
                           :label (label basis-mulf)))
           ;; Recursive case.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (reschild basis-child)
                                (apply-polarity-with-divergences
                                  reschild
                                  basis-child
                                  basis-divergence-point
                                  mon-mulf
                                  in-divergence))
                            (expr reslulf)
                            (expr basis-mulf))
              :label (label basis-mulf))))))
     (get-antecedent-pred-mon (mon-mulf)
       "Gets the antecedent predicate of a monotonicity rule mulf."
       (second (expr (first (expr mon-mulf)))))
     (get-consequent-pred-mon (mon-mulf)
       "Gets the consequent predicate of a monotonicity rule mulf."
       ;; ((<det> <pred1>) ((<tense> be.v) <pred2>))
       (second (expr (second (expr mon-mulf)))))
     (common-ancestor (lulf points)
       "Returns the index of the lowest common ancestor of the given list of points."
       ;; Returns a list of completed points if incomplete a number for the index if complete.
       (let* ((curres (if (member (label lulf) points)
                       (list (label lulf))
                       nil))
              (all-recres curres))
         (cond
           ((atom (expr lulf)) curres)
           (t
            (loop for child in (expr lulf)
                  do
                  (let ((recres (common-ancestor child points)))
                    (if (numberp recres)
                      ;; Recursive result is complete, just go back up.
                      (return-from common-ancestor recres)
                      ;; Add recursive result.
                      (setf all-recres (append all-recres recres)))))
            (setf all-recres (remove-duplicates all-recres))
            (if (= (length all-recres) (length points))
              (label lulf)
              all-recres))))))
  ;; Labels body.
  (let*
    ((reslulf (dfs-label-tree ulf))
     (srclulfs (mapcar #'dfs-label-tree src-ulfs))
     (srcmulfs (mapcar #'get-infer-nlog-marked-ulf src-ulfs))
     (src-divergences
       (mapcar #'(lambda (srclulf) (find-divergences reslulf srclulf))
               srclulfs))
     (basis-idx
       (cond
         ;; Basis is the non monotonicity rule format, if only one fits that.
         ((and (mon-form? (first src-ulfs))
               (not (mon-form? (second src-ulfs))))
          1)
         ((and (not (mon-form? (first src-ulfs)))
               (mon-form? (second src-ulfs)))
          0)
         ;; Otherwise, the basis is the one with fewer divergences.
         ((<= (length (first src-divergences))
              (length (second src-divergences)))
          0)
         (t 1)))
     (mon-idx (- 1 basis-idx))
     (basis-mulf (nth basis-idx srcmulfs))
     (basis-mlulf (build-multi-labeled-tree (nth basis-idx srclulfs)
                                            (list basis-mulf)
                                            '(polarity)))
     (basis-divergences (nth basis-idx src-divergences))
     (basis-divergence-point (common-ancestor reslulf
                                              (mapcar #'car basis-divergences)))
     (basis-divergence-src-pt (common-ancestor (nth basis-idx srclulfs)
                                               (mapcar #'cdr basis-divergences)))
     (mon-mulf (if (eql '- (get-label-from-key basis-mlulf
                                                    'polarity
                                                    basis-divergence-src-pt))
                      (get-antecedent-pred-mon (nth mon-idx srcmulfs))
                      (get-consequent-pred-mon (nth mon-idx srcmulfs)))))
    (labeled-tree-to-marked-ulf
      (apply-polarity-with-divergences
        reslulf
        basis-mulf
        basis-divergence-point
        mon-mulf
        nil)))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'umi-positive-polarity *infer-nlog-marking-dispatch-fn-ht*)
      #'umi-marking-dispatch-fn)
(setf (gethash 'umi-negative-polarity *infer-nlog-marking-dispatch-fn-ht*)
      #'umi-marking-dispatch-fn)


;;;
;;; Negations
;;;

(defun neg-elim (ulf)
  (if (ttt:match-expr '((!1 not not.adv-s) ((!2 not not.adv-s) _!3))
                      ulf)
    (list (second (second ulf)))
    nil))

(defun neg-intro (ulf)
  (if (not (ttt:match-expr '((!1 not not.adv-s) ((!2 not not.adv-s) _!3))
                           ulf))
    (list (list 'not (list 'not ulf)))))

(defun replace-ab (a b ulf)
  (labels
    ((idx-recurse (i)
       (let ((recval (replace-ab a b (nth i ulf)))
             (pre (subseq ulf 0 i))
             (post (subseq ulf (1+ i))))
         (mapcar (lambda (x) (append pre (list x) post)) recval))))
    ; labels body
    (cond
      ((equal a ulf) (list b))
      ((atom ulf) nil)
      (t
        (apply #'append
               (mapcar (lambda (i) (idx-recurse i)) (range (length ulf))))))))

(defun no2notindef (ulf)
  (let ((no2indefs (apply #'append
                          (mapcar #'(lambda (indef)
                                      (replace-ab 'no.d indef ulf))
                                  '(some.d a.d an.d)))))
    ; Add 'not at the top-level of all the formulas.
    (mapcar #'(lambda (newulf) (list 'not newulf))
            no2indefs)))
(defun no2notindef-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from no2notindef."
  ;; 1. Strip top-level not from the result.
  ;; 2. Map over all of the same polarities.
  ;; 3. Make the top-level polarity negative and then add the not back in with
  ;;    a positive polarity there.
  (assert (= 1 (length src-ulfs)))
  (labels
    ((transfer-polarities (ulf srcmulf)
       "Builds a marked ULF with `ulf` values and `srcmulf` polarities.
       Both must have the same shape."
       (cond
         ;; Base case.
         ((atom ulf)
          (make-instance 'labeled-tree
                         :expr ulf
                         :label (global-polarity srcmulf)))
         ;; Recursive case.
         (t
          (make-instance
            'labeled-tree
            :expr (mapcar #'transfer-polarities ulf (get-ulf srcmulf))
            :label (global-polarity srcmulf))))))
    ;; Main body.
    (labeled-tree-to-marked-ulf
      (let ((core-form
              (transfer-polarities
                (second ulf)
                (get-infer-nlog-marked-ulf (first src-ulfs)))))
        (setf (label core-form) '-)
        (make-instance
          'labeled-tree
          :expr (list (make-instance 'labeled-tree :expr 'not :label '+)
                      core-form)
          :label '+)))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'no2notindef *infer-nlog-marking-dispatch-fn-ht*)
      #'no2notindef-marking-from-source-dispatch)

