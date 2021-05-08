;;; Inference rules that extract monotonicity rules from ULF formulas.

(in-package :ulf-fracas)


(defun select-forall! (np)
  "Given a noun phrase, selects the forall determiner. Plural and mass nouns
  get \"all\", singular count nouns get \"every\""
  (let ((hn (ulf:find-np-head np :callpkg :ulf-fracas)))
    (cond
      ((listp hn) 'all.d)
      (t 'every.d))))

(defparameter *ttt-intersective-n+preds-rules*
  '((/ (n+preds (!1 ~ relativized-sent?) _+2 _!3)
       (((select-forall! !1) (n+preds !1 _+2 _!3))
        ((pres be.v) (n+preds !1 _+2))))
    (/ (n+preds (!1 ~ relativized-sent?) _!2 _+3)
       (((select-forall! !1) (n+preds !1 _!2 _+3))
        ((pres be.v) (n+preds !1 _+3))))
    ;; Most basic cases.
    (/ (n+preds (!1 ~ relativized-sent?) _!2)
       (((select-forall! !1) (n+preds !1 _!2)) ((pres be.v) !1)))
    (/ (n+preds _!1 (!2 ~ relativized-sent?))
       (((select-forall! _!) (n+preds _!1 !2)) ((pres be.v) !2)))))

(defun make-intersective-rule-fluent (ulf)
  "Makes a fluent version of an auto-generated intersective rule of the form
  ((every.d A) ((tense be.v) B)). If be.v is followed by a noun, generate an
  indefinite and add the equal sign."
  (ttt:apply-rule
    '(/ ((lex-tense? be.v) (!1 noun? ~ (= ((! lex-det? k) noun?))))
        ((lex-tense? be.v) (= ((get-copula-indef! !1) !1))))
    ulf))

(defun intersective-n+preds (ulf)
  "From the existence of (n+preds pred1 pred2) infer
   \"Every (n+preds pred1 pred2) is a pred1\" and
   \"Every (n+preds pred1 pred2) is a pred2\"
  Alternatively, we could just write a general function that directly performs
  this substitution, but this unifies the inferences more and allows
  inferences to go in either direction."
  (let ((rawresults (apply #'append
                           (mapcar #'(lambda (rule)
                                       (ttt-all-rule-results rule ulf))
                                   *ttt-intersective-n+preds-rules*))))
    (mapcar #'make-intersective-rule-fluent rawresults)))

(defun find-and-apply-subexpr-polarities (reslulf idx-corr srcmlulf top-polarity)
  "Generates polarity labeling of reslulf given the top-level polarity and
  index correspondence. The index correspondence is the only segment that
  should enable polarity changes so perform that then fill in the rest with
  top-polarity."
  ;; Recurse and if part of the index correspondence, use the
  ;; corresponding polarities with enforced top-polarity. Otherwise, just
  ;; put the top-polarity.
  (cond
    ;; Found it.
    ((assoc (label reslulf) idx-corr)
     (let* ((src-index (cdr (assoc (label reslulf) idx-corr)))
            (src-pulf (pull-labeled-tree-at-index srcmlulf
                                                  'polarity
                                                  src-index)))
       (if (eql top-polarity (get-label-from-key srcmlulf
                                                 'polarity
                                                 src-index))
         src-pulf
         (flip-lulf-polarity src-pulf))))
    ;; Base case.
    ((atom (expr reslulf))
     (make-instance 'labeled-tree
                    :expr (expr reslulf)
                    :label top-polarity))
    ;; Recursive case.
    (t
     (make-instance 'labeled-tree
                    :expr (mapcar #'(lambda (child)
                                      (find-and-apply-subexpr-polarities
                                        child idx-corr srcmlulf top-polarity))
                                  (expr reslulf))
                    :label top-polarity))))

(defun apply-polarity-with-correspondence (reslulf idx-corr srcmlulf)
  "Given the index correspondence of the core intersective modification,
  applies the complete polarity to the resulting lulf." (let
    ((restrictor (find-and-apply-subexpr-polarities
                   (second (expr (first (expr reslulf))))
                   idx-corr
                   srcmlulf
                   '-))
     (body (find-and-apply-subexpr-polarities
             (second (expr reslulf))
             idx-corr
             srcmlulf
             '+))
     ;; Determiner should have no correspondences, so simply mark with positive polarity.
     (determiner (find-and-apply-subexpr-polarities
                   (first (expr (first (expr reslulf))))
                   idx-corr
                   srcmlulf
                   '+))
     subject)
    ;; Put it together.
    (setf subject
          (make-instance 'labeled-tree
                         :expr (list determiner restrictor)
                         :label '+))
    (make-instance 'labeled-tree
                   :expr (list subject body)
                   :label '+)))

(defun intersective-n+preds-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from intersective-n+preds."
  ;; 1. Find the segments of the source corresponding to the two predicates.
  ;; 2. Flip or not depending on the top-level predicate polarity (pred1 and
  ;;    pred2) should be negative at top-level for the antecedent and positive
  ;;    for consequent.
  ;;
  ;; `ulf` is be of the form ((det pred1) ((tense be.v) pred2)) where pred1 and
  ;; pred2, modulo indefinites and equal signs (all polarity preserving) exist
  ;; in the source ulf.
  ;; 
  ;; pred1 is an n+preds which also appears in the source sentence. Use this
  ;; information to get a correspondence between the source result formulas.
  (assert (= 1 (length src-ulfs)))
  (labels
    ((get-n+preds-correspondence (reslulf srclulf)
       "Given the source and result labeled ULF, returns a list of index
       correpondence of the core n+preds."
       ;; Get the n+preds expression from the result lulf, which will be the restrictor of the subject.
       (let*
         ((npreds-reslulf (second (expr (first (expr reslulf)))))
          (npreds-srclulf (find-expr (unlabeled-tree npreds-reslulf) srclulf)))
         ;; Extract out the corresponding labels into an alist.
         (mapcar #'cons
                 (mapcar #'label (cdr (expr npreds-reslulf)))
                 (mapcar #'label (cdr (expr npreds-srclulf)))))))
    ;; Main body of labels
    (let*
      ((srcmulf (get-infer-nlog-marked-ulf (first src-ulfs)))
       (srclulf (dfs-label-tree (first src-ulfs)))
       (reslulf (dfs-label-tree ulf))
       ;; Build the multi-label tree for the source.
       (srcmlulf (build-multi-labeled-tree
                   srclulf
                   (list srcmulf)
                   '(polarity))))
      ;; Apply the polarity using this index correspondence for non-trivial parts.
      (labeled-tree-to-marked-ulf
        (apply-polarity-with-correspondence
          reslulf
          (get-n+preds-correspondence reslulf srclulf)
          srcmlulf)))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'intersective-n+preds
               *infer-nlog-marking-dispatch-fn-ht*)
      #'intersective-n+preds-marking-from-source-dispatch)

(defparameter *ttt-intersective-noun-modification-rules*
  '((/ ((!1 adj? noun? ~ non-subsective-adj?) (!2 noun?))
       (((select-forall! !2) (!1 !2)) ((pres be.v) !2)))))

(defun intersective-noun-modification (ulf)
  "From the existence of modification of a noun (mod noun) infers the rule
  Every (mod noun) is a noun."
  (mapcar #'make-intersective-rule-fluent
          (apply #'append
                 (mapcar #'(lambda (rule)
                             (ttt-all-rule-results rule ulf))
                         *ttt-intersective-noun-modification-rules*))))

(defun intersective-noun-modification-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from
  intersective-noun-modification."
  ;; 1. Find the segments of the source corresponding to the two predicates.
  ;; 2. Flip or not depending on the top-level predicate polarity (pred1 and
  ;;    pred2) should be negative at top-level for the antecedent and positive
  ;;    for consequent.
  ;;
  ;; `ulf` is be of the form ((det pred1) ((tense be.v) pred2)) where pred1 and
  ;; pred2, modulo indefinites and equal signs (all polarity preserving) exist
  ;; in the source ulf.
  ;; 
  ;; pred1 is an intersective noun modification which also appears in the
  ;; source sentence. Use this information to get a correspondence between the
  ;; source result formulas.
  (assert (= 1 (length src-ulfs)))
  (labels
    ((get-noun-phrase-correspondence (reslulf srclulf)
       "Given the source and result labeled ULF, returns a list of index
       correpondence of the core intersective noun phrase."
       ;; Get the n+preds expression from the result lulf, which will be the restrictor of the subject.
       (let*
         ((np-reslulf (second (expr (first (expr reslulf)))))
          (np-srclulf (find-expr (unlabeled-tree np-reslulf) srclulf)))
         ;; Extract out the corresponding labels into an alist.
         (mapcar #'cons
                 (mapcar #'label (cdr (expr np-reslulf)))
                 (mapcar #'label (cdr (expr np-srclulf)))))))
    ;; Main body of labels
    (let*
      ((srcmulf (get-infer-nlog-marked-ulf (first src-ulfs)))
       (srclulf (dfs-label-tree (first src-ulfs)))
       (reslulf (dfs-label-tree ulf))
       ;; Build the multi-label tree for the source.
       (srcmlulf (build-multi-labeled-tree
                   srclulf
                   (list srcmulf)
                   '(polarity))))
      ;; Apply the polarity using this index correspondence for non-trivial parts.
      (labeled-tree-to-marked-ulf
        (apply-polarity-with-correspondence
          reslulf
          (get-noun-phrase-correspondence reslulf srclulf)
          srcmlulf)))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'intersective-noun-modification
               *infer-nlog-marking-dispatch-fn-ht*)
      #'intersective-noun-modification-marking-from-source-dispatch)

