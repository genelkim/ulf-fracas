
(in-package :ulf-fracas)

;;; Inferences for conservativity.
;;; We follow the language used by FraCaS:
;;;   Q A B -> Q A is A who/that B
;;;
;;; Use rel.rel for relativizer with unknown personhood.  We can simply infer
;;; who.rel and that.rel from it. Later we can include propagation of
;;; assumptions (if we see who.rel we know personhood).


(defun get-tense! (vp)
  "Gets the tense from a verb phrase, defaulting to the leftmost element in a
  coordinated verb phrase. If no tense if found, returns pres by default."
  (cond
    ;; Take the first in a coordinated VP
    ((and (listp vp) (some #'lex-coord? vp))
     (get-tense! (first (remove-if-not #'(lambda (x) (or (verb? x)
                                                         (tensed-verb? x)))))))
    ;; General case, find head verb and get its tense.
    (t
      (let ((hv (ulf:find-vp-head vp :callpkg :ulf-fracas)))
        (if (or (tensed-verb? hv) (tensed-aux? hv))
          (first hv)
          'pres)))))

(defun find-first-leaf (tree fn)
  "Gets the first leaf of the given s-expression that satisfies the function fn
  according to DFS traversal. nil if none is found."
  (cond
    ;; Base case.
    ((atom tree) (if (funcall fn tree) tree nil))
    ;; Recursive case.
    (t (let ((recval (find-first-leaf (car tree) fn)))
         (if recval
           recval
           (find-first-leaf (cdr tree) fn))))))

(defun get-copula-indef! (np)
  "Computes the indefinite determiner for the argument of a copula for the
  given noun phrase. For plurals, this uses a kind operator, e.g.
    \"Some dogs are {k} dogs with brown fur\""
  (cond
    ;; Plural, so use kind.
    ((and (noun? np) (listp (ulf:find-np-head np :callpkg :ulf-fracas)))
     'k)
    ;; Not plural so just get indefinite based on first surface symbol.
    (t
     (ulf:add-suffix
       (util:indefinite-article
         (nth-value 0
                    (ulf:split-by-suffix
                      (find-first-leaf np #'ulf:surface-token?))))
       'd :pkg :ulf-fracas))))

(defun copula-indef? (sym)
  "Returns whether the given symbol can act as an indefinite determiner for the
  argument of a copula."
  (member sym '(a.d an.d k)))

(defun can-generate-conservativity? (ulf)
  "Checks whether we can apply the simple conservativity rule without risking
  infinite recursion.
    ((<det> <noun1>) <verb>)
    -> ((<det> <noun1>) is (<det> (n+preds <pred1> (who.rel/that.rel <verb>))))
  If <verb> matches ((<tense> be.v) (= (<indefinite determiner> <arg-noun>)))
  where <arg-noun> is subsumed by <noun1>, then no inference is made since it's
  redundant and would lead to infinite recursion."
  ;; Pull apart the ULF.
  (let* ((conservativity-antecedent-ttt '((det? pred?) (!2 verb? tensed-verb?)))
         (det (ttt:apply-rule (list '/ conservativity-antecedent-ttt 'det?)
                              ulf :shallow t :max-n 1))
         (noun (ttt:apply-rule (list '/ conservativity-antecedent-ttt 'pred?)
                               ulf :shallow t :max-n 1))
         (vp (ttt:apply-rule (list '/ conservativity-antecedent-ttt '!2)
                             ulf :shallow t :max-n 1))
         (pred-arg (ttt:apply-rule '(/ ((lex-tense? be.v) (= (copula-indef? _!)))
                                       _!)
                     vp :shallow t :max-n 1)))
    ; If the TTT rule didn't extract properly, fail.
    (when (or (equal det ulf) (equal noun ulf) (equal vp ulf) (equal pred-arg ulf))
      (return-from can-generate-conservativity? nil))
    (cond
      ;; Not the right format, so can't
      ((equal pred-arg vp)
       t)
      ;; n+preds of both the noun and pred-arg
      ;; subsumed if all are part of the subject noun.
      ((and (listp noun) (eql 'n+preds (first noun))
            (listp pred-arg) (eql 'n+preds (first pred-arg)))
       (< (length (intersection pred-arg noun :test #'equal))
          (min (length pred-arg) (length noun))))
      ;; n+preds for noun only
      ((and (listp noun) (eql 'n+preds (first noun)))
       (not (member pred-arg noun :test #'equal)))
      ;; pred-arg only n+preds -- never subsumes
      ((and (listp pred-arg) (eql 'n+preds (first pred-arg)))
       (not (member noun pred-arg :test #'equal)))
      ;; Neither are n+preds, just check equality
      (t
       (not (equal noun pred-arg))))))

(defun simple-conservativity-map! (ulf)
  "Generates conservativity inference for a single known matching instance
  without checking any validity. Use use-generate-conservativity? to check for
  viable formulas that won't lead to infinite recursion.
  ((<det> <noun>) <verb>)
  -> ((<det> <noun>) is (<det> (n+preds <noun> (who.rel/that.rel <verb>))))
  "
  ;; TTT rule for inference without the overlap check.
  (ttt:apply-rule
    '(/ ((det? pred?) (!2 verb? tensed-verb?))
        ((det? pred?)
         (((get-tense! !2) be.v)
          (= ((get-copula-indef! pred?)
              (n+preds pred? (who.rel !2)))))))
    ulf :shallow t :max-n 1))

(defparameter *ttt-conservativity*
  '(/ (!1 can-generate-conservativity?)
      (simple-conservativity-map! !1)))

(defun existential-there-conservativity-map! (ulf)
  "Generates a conservativity inference in the form of existential-there
  assuming the correct format. This inference is divided into two categories,
  existence and non-existence depending on the extension of the quantifier's
  interpretation.
    ((<det> <noun>) <verb>) {not}[for non-existence quantifiers]
    -> (there is (a (<noun> (which/that/who <verb>)))"
  (let (;; TTT rule for inference without the overlap check.
        (pred
          (ttt:apply-rule '(/ ((det? pred?) (!2 verb? tensed-verb?))
                              (= ((get-copula-indef! pred?)
                                  (n+preds pred? (who.rel !2)))))
                          ulf :shallow t :max-n 1))
        (tense
          (ttt:apply-rule '(/ ((det? pred?) (!2 verb? tensed-verb?))
                              (get-tense! !2))
                          ulf :shallow t :max-n 1))
        (det
          (ttt:apply-rule '(/ ((det? pred?) _!) det?)
                          ulf :shallow t :max-n 1))
        result)
    (setf result
          (list 'there.pro
                (append
                  (list (list tense 'be.v))
                  ;(if (zero-max-quantifier? det) '(not) nil) ; natural negation
                  (list pred))))
    (if (zero-max-quantifier? det)
      (list 'not result) ; un-natural but simple inference negation
      result)))

(defparameter *ttt-existential-there-conservativity*
  '(/ (!1 can-generate-conservativity?)
      (existential-there-conservativity-map! !1)))

(defun conservativity-inference (ulf)
  (apply #'append
         (mapcar #'(lambda (ttt-rule)
                     (ttt-apply-rule-possibilities
                       ttt-rule
                       ulf :max-per-tree 1 :min-per-tree 1))
                 (list *ttt-conservativity*
                       *ttt-existential-there-conservativity*))))

(defun conservativity-inference-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from
  conservativity-inference."
  ;; 1. Get the two predicates from the source ulf, which is of the form:
  ;;    ((<det> <noun>) <verb>)
  ;; 2. Get the index correspondences between the two.
  ;; 3a. For "there is" sentences assume the top-level of everything is positive.
  ;; 3b. For the quantifier ones, I get the polarity of the restrictor and body and preserve that.
  (assert (= 1 (length src-ulfs)))
  (labels
    ((lowest-common-ancestor-of-conservativity-form (nodelabels srclulf)
       "Lowest common ancestor of nodelabels with form ((<det> _!) _!)"
       (let (curlabels)
         (when (member (label srclulf) nodelabels)
           (setf curlabels (list (label srclulf))))

         (cond
           ;; Base case, return current result.
           ((atom (expr srclulf)) (values nil curlabels))
           ;; Recursive case, merge results.
           ;; If all labels are contained and form is right, return.
           (t
            (loop for child in (expr srclulf) do
                  (let ((recres (multiple-value-list
                                  (lowest-common-ancestor-of-conservativity-form
                                    nodelabels child))))
                    ;; If we have a result, just return that.
                    (when (not (null (first recres)))
                      (return-from lowest-common-ancestor-of-conservativity-form
                                   (values-list recres)))
                    ;; Otherwise update the current labels.
                    (setf curlabels (append curlabels (second recres)))))
            ;; If we have all the labels and the right form return current lulf.
            (if (and (every #'(lambda (lbl) (member lbl curlabels))
                            nodelabels)
                     (and (can-generate-conservativity?
                            (unlabeled-tree srclulf))))
              (values srclulf curlabels)
              ;; Otherwise just return the labels.
              (values nil curlabels))))))
     (find-conservativity-application-location (reslulf srclulf)
       "Finds where the conservativity rule was applied.
       1. Find where the formulas diverge.
       2. Find the deepest index that is an ancestor of all divergences and the
          source ulf is of the form ((<det> <noun>) <verb>)"
       (let ((divergences (find-divergences reslulf srclulf)))
         (lowest-common-ancestor-of-conservativity-form
           (mapcar #'cdr divergences)
           srclulf)))
     (extract-correspondence (reslulf srclulf)
       "Extracts the correspondence between the result and source assuming
       we've found the correct quantifier in the source that corresponds to the
       result."
       ;; Get the notable predicates, and get a correspondence.
       (let* ((src-lrests (list (second (expr (first (expr srclulf))))))
              (src-lbody (second (expr srclulf)))
              (res-lbody (find-expr (unlabeled-tree src-lbody) reslulf))
              res-lrests)
         ;; When the restrictor is n+preds, this might get more inserted to correspond the inner predicates.
         (when (and (listp (expr (first src-lrests)))
                    (eql 'n+preds (expr (first (expr (first src-lrests))))))
           (setf src-lrests
                 (cdr (expr (first src-lrests)))))
         (setf res-lrests
               (mapcar #'(lambda (src-lrest)
                           (find-expr (unlabeled-tree src-lrest) reslulf))
                       src-lrests))
         (cons (cons (label res-lbody) (label src-lbody))
               (mapcar #'cons
                       (mapcar #'label res-lrests)
                       (mapcar #'label src-lrests)))))
     (get-correspondence (reslulf srclulf)
       "Given the resulting and source labeled ULFs, find the source quantifier
       and extracts the correspondence."
       ;; srclulf cannot be of the right form ((a b) c)
       (when (or (atom (expr srclulf))
                 (not (= 2 (length (expr srclulf))))
                 (atom (expr (first (expr srclulf))))
                 (not (= 2 (length (expr (first (expr srclulf)))))))
         (return-from get-correspondence nil))
       ;; We've found it if we can find the restrictor and body expressions in
       ;; the result, but not the whole thing.
       (let* ((src-lrests (list (second (expr (first (expr srclulf))))))
              (src-lbody (second (expr srclulf)))
              (res-lbody (find-expr (unlabeled-tree src-lbody) reslulf))
              (res-whole (find-expr (unlabeled-tree srclulf) reslulf))
              res-lrests)
         ;; When the restrictor is n+preds, this might get more inserted to correspond the inner predicates.
         (when (and (listp (expr (first src-lrests)))
                    (eql 'n+preds (expr (first (expr (first src-lrests))))))
           (setf src-lrests
                 (cdr (expr (first src-lrests)))))
         (setf res-lrests
               (mapcar #'(lambda (src-lrest)
                           (find-expr (unlabeled-tree src-lrest) reslulf))
                       src-lrests))
         (cond
           ;; Found the correspondence, extract.
           ((and (not (null res-lrests))
                 (not (null res-lbody))
                 (null res-whole))
            (extract-correspondence reslulf srclulf))
           ;; Not found, recurse. NB: Base case handled at top of function.
           (t
            (loop for srcchild in (expr srclulf)
                  do (let ((recres (get-correspondence reslulf srcchild)))
                       (when (not (null recres))
                         (return-from get-correspondence recres))))))))
     (apply-polarity-for-there-is (reslulf srcmlulf idx-corr)
       "Applies the polarity to the resulting for existential-there sentence.
       The polarity is based on the source multi-labeled ulf (with index and
       polarity) and an index correspondence between the source and result
       formulas."
       ;; Everything is positive at the top, flip if the subtree are not that way.
       (let ((idx (label reslulf))
             src-pulf)
         (cond
           ;; In a correspodence.
           ((assoc idx idx-corr)
            (setf src-pulf
                  (pull-labeled-tree-at-index srcmlulf
                                              'polarity
                                              (cdr (assoc idx idx-corr))))
            (if (eql '+ (label src-pulf))
              src-pulf
              (flip-lulf-polarity src-pulf))) ; Flip if top is not positive.
           ;; At a leaf, base case.
           ((atom (expr reslulf))
            (make-instance
              'labeled-tree
              :expr (expr reslulf)
              :label '+))
           ;; Recursive case.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (child)
                                (apply-polarity-for-there-is
                                  child srcmlulf idx-corr))
                            (expr reslulf))
              :label '+)))))
     (basic-quantifier-recfn (reslulf srcmlulf idx-corr top-polarity)
       "Subroutine for apply-polarity-for-basic-quantifier which includes a
       polarity correspondence from src index to the expected top-level
       polarity."
       (let ((idx (label reslulf)))
         (cond
           ;; In a correspondence.
           ((assoc idx idx-corr)
            (setf src-pulf
                  (pull-labeled-tree-at-index srcmlulf
                                              'polarity
                                              (cdr (assoc idx idx-corr))))
            (if (eql top-polarity (label src-pulf))
              src-pulf
              (flip-lulf-polarity src-pulf)))
           ;; At leaf.
           ((atom (expr reslulf))
            (make-instance 'labeled-tree :expr (expr reslulf) :label top-polarity))
           ;; Recursive case.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (child)
                                (basic-quantifier-recfn
                                  child srcmlulf idx-corr top-polarity))
                            (expr reslulf))
              :label top-polarity)))))
     (apply-polarity-for-basic-quantifier (reslulf srcmlulf idx-corr)
       "Applies the polarity to the resulting for basic quantified sentences.
       The polarity is based on the source multi-labeled ulf (with index and
       polarity) and an index correspondence between the source and result
       formulas."
       ;; 1. Get the top-level polarity for the restrictor and body of the source.
       ;; 2. Preserve that in the new result.
       (let*
         ((srcmulf (pull-labeled-tree srcmlulf 'polarity))
          (src-rest-pol (label (second (expr (first (expr srcmulf))))))
          (src-body-pol (label (second (expr srcmulf))))
          ;; Determiner which is simply copied from the source.
          (determiner (first (expr (first (expr srcmulf))))))
         (make-instance
           'labeled-tree
           :expr (list (make-instance
                         'labeled-tree
                         :expr (list determiner
                                     (basic-quantifier-recfn
                                       (second (expr (first (expr reslulf))))
                                       srcmlulf
                                       idx-corr
                                       src-rest-pol))
                         :label '+)
                       (basic-quantifier-recfn
                         (second (expr reslulf))
                         srcmlulf
                         idx-corr
                         src-body-pol))
           :label '+)))
     (apply-polarity-at-location (reslulf srcmlulf full-srcmlulf srcloc-index)
       "Traverses reslulf and srcmlulf mapping polarities directly until the
       srcloc-index location at which point the conservativity mapping is
       applied."
       (let (ulf srclulf idx-corr srcmltree)
         (cond
           ;; At location of application.
           ((eql (index srcmlulf) srcloc-index)
            (setf ulf (unlabeled-tree reslulf))
            (setf srclulf (pull-index-tree-at-index full-srcmlulf srcloc-index))
            (setf idx-corr (get-correspondence reslulf srclulf))
            (setf srcmltree (build-tree-from-node srcmlulf full-srcmlulf))
            (cond
              ((member (first ulf) '(not not.adv-s))
               ;; Top-level negation.
               ;; Recurse into rest and then flip the whole this and add a not at the top.
               (make-instance
                 'labeled-tree
                 :expr (list (make-instance 'labeled-tree :expr 'not :label '+)
                             (flip-lulf-polarity
                               (apply-polarity-for-there-is (second (expr reslulf))
                                                            srcmltree
                                                            idx-corr)))
                 :label '+))
              ((eql 'there.pro (first ulf))
               ;; there is sentence handling
               (apply-polarity-for-there-is reslulf
                                            srcmltree
                                            idx-corr))
              ;; basic quantifier handling
              (t (apply-polarity-for-basic-quantifier
                   reslulf srcmltree idx-corr))))
           ;; Base case.
           ((atom (expr reslulf))
            (make-instance 'labeled-tree
                           :expr (expr reslulf)
                           :label (get-label-from-key full-srcmlulf
                                                      'polarity
                                                      (index srcmlulf))))
           ;; Recursive case.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar
                      #'(lambda (reschild srcchild)
                          (apply-polarity-at-location
                            reschild srcchild full-srcmlulf srcloc-index))
                      (expr reslulf)
                      (expr srcmlulf))
              :label (get-label-from-key full-srcmlulf
                                         'polarity
                                         (index srcmlulf))))))))
    ;; Main body of labels.
    (let*
      ((srclulf (dfs-label-tree (first src-ulfs)))
       (reslulf (dfs-label-tree ulf))
       ;; Build the multi-label tree.
       (srcmulf (get-infer-nlog-marked-ulf (first src-ulfs)))
       (srcmlulf (build-multi-labeled-tree
                   srclulf
                   (list srcmulf)
                   '(polarity)))
       (conservativity-loc-srclulf
         (find-conservativity-application-location reslulf srclulf)))

      ;; Body of let*
      (labeled-tree-to-marked-ulf
        (apply-polarity-at-location reslulf
                                    (root srcmlulf)
                                    srcmlulf
                                    (label conservativity-loc-srclulf))))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'conservativity-inference
               *infer-nlog-marking-dispatch-fn-ht*)
      #'conservativity-inference-marking-from-source-dispatch)

