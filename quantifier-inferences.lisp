
(in-package :ulf-fracas)

(defun replace-suffix! (sym new-suffix)
  (multiple-value-bind (lemma _) (split-by-suffix sym)
    (declare (ignore _))
    (add-suffix lemma new-suffix)))
(defparameter *determiners*
  '(all every some a an many most much few several the no that which my her his your their one neither both))

(defun adj-det? (sym)
  "Returns t if the symbol is a possible determiner with an adjective suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'a)
         (member lemma *determiners*))))

;; Positive polarity quantifier mappings.
;; TTT rules are wrapped in a list '(ttt <rule>). 
(defparameter *quantifier-relations*
  '((the.d . a.d)
    (all.d . a.d)
    (every.d . a.d)
    (each.d . a.d)
    (both.d . a.d)
    ))

(defparameter *ttt-quantifier-relations*
  '(((/ lex-det?
        (nquan (at_least.mod-a (replace-suffix! lex-det? a))))
     .
     (/ (nquan (at_least-mod-a adj-det?))
        (replace-suffix! adj-det? d)))))

(defun quantifier-polarity (inulf &key (callpkg nil))
  "Generates inferences by substituting quantifiers along known generalizations
  and specializations."
  (labels
    ((quantifier-ttt-rules (rules expr)
       (remove-if #'null
                  (mapcar #'(lambda (r)
                              (apply-rule r expr :max-n 1 :shallow t))
                          rules)))
     (rechelper (uulf mulf)
       "Recursive heavy-lifting helper function.
         uulf : unmarked ULf
         mulf : marked ULF
         pol  : current polarity

       Returns a list of ULFs with all determiner substitutions.
       "
       (let (result)
         (cond
           ;; Positive polarity.
           ((and (det? uulf)
                 (equal (global-polarity mulf) '+)
                 (or (setf results
                           (quantifier-ttt-rules (mapcar #'car *ttt-quantifier-relations*)
                                                 uulf))
                 (assoc uulf *quantifier-relations*)))
            (remove-if #'null
                       (append (list (cdr (assoc uulf *quantifier-relations*)))
                               results)))
           ;; Negative polarity.
           ((and (det? uulf)
                 (equal (global-polarity mulf) '-)
                 (or (setf results
                           (quantifier-ttt-rules (mapcar #'cdr *ttt-quantifier-relations*)
                                                 uulf))
                     (rassoc uulf *quantifier-relations*)))
            (remove-if #'null
                       (append (list (car (rassoc uulf *quantifier-relations*)))
                               results)))
           ;; Recursive case.
           ;; Recurse into each constituent and reconstruct the rest.
           ((listp uulf)
            (apply #'append
                   (mapcar #'(lambda (idx) (idx-recurse idx uulf (get-ulf mulf)))
                           (range (length uulf)))))
           ;; Uninteresting atomic case.
           (t nil))))
     (idx-recurse (idx uulfs mulfs)
       "Given a list of ULFs (both marked and unmarked counterparts) and an
       index, recurses into that index and inserts that into the list."
       (let ((recvals (rechelper (nth idx uulfs) (nth idx mulfs)))
             (preslice (subseq uulfs 0 idx))
             (postslice (subseq uulfs (1+ idx))))
         (mapcar #'(lambda (newval) (append preslice (list newval) postslice))
                 recvals)))
       ) ; end of labels defs
    ;; labels body
    (inout-intern (inulf ulf :ulf-fracas :callpkg callpkg)
      (rechelper ulf (get-infer-nlog-marked-ulf ulf)))))

(defun quantifier-polarity-inference-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from quantifier-polarity."
  (assert (= 1 (length src-ulfs)))
  ;; 1. Get the polarity of source ULF
  ;; 2. Get the scoping for the source ULF
  ;; 3. Get the local polarity marking of the source scoping.
  ;; 4. Find the replaced quantifier in the global marked ULF.
  ;; 5. Replace the quantifier in local marking scoped formula.
  ;; 6. Geneate the global polarity marking from the replaced formula.
  (let*
    ((srculf (first src-ulfs))
     (reslulf (dfs-label-tree ulf))
     ;; 1. Get the polarity of source ULF
     (srcmulf (get-infer-nlog-marked-ulf srculf))
     ;; 2. Get the scoping for the source ULF
     (src-scope-info (multiple-value-list
                       (infer-scoping-from-polarity srcmulf)))
     (src-scope-alist (first src-scope-info))
     (srclulf (second src-scope-info))
     (srclslf (third src-scope-info))
     (srcmslf (fourth src-scope-info))
     ;; 3. Get the local polarity marking of the source scoping.
     (src-local-mslf (fifth src-scope-info))
     ;; 4. Find the replaced quantifier in the global marked ULF.
     ;; Since the ULFs should only differ at the quantifier, this will be an
     ;; alist with a single entry from the result label to the source label.
     (divergences (find-divergences reslulf srclulf))
     new-quant reslslf)

    (assert (= 1 (length divergences))
            (divergences reslulf srclulf)
            "divergences: ~s~%reslulf: ~s~%srclulf: ~s~%." divergences reslulf srclulf)
    (setf new-quant (get-node-at-label reslulf
                                       (car (first divergences))))
    ;; 5. Replace the quantifier in SLF.
    (setf reslslf (replace-expr-at-label srclslf
                                         (cdr (first divergences))
                                         (expr new-quant)))
    ;; 6. Geneate the global polarity marking from the replaced formula.
    (infer-remaining-ulf-polarities
      (slf-polarities-to-ulf 
        reslslf
        (globalize-slf-polarity
          (mark-local-slf-polarity (unlabeled-tree reslslf)))
        reslulf))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'quantifier-polarity *infer-nlog-marking-dispatch-fn-ht*)
      #'quantifier-polarity-inference-marking-from-source-dispatch)

;; Taken from EPILOG
(defparameter *quantifier-polarities*
  '((all.d . (- +))
    (every.d . (- +))
    (each.d . (- +))
    (some.d . (+ +))
    (several.d . (+ +))
    (a.d . (+ +))
    (an.d . (+ +))
    (the.d . (o +))
    (most.d . (o +))
    (many.d . (+ +))
    (few.d . (- -))
    (a_few.d . (+ +))
    (no.d . (- -))))

(defun det-polarity-accessor (det accessfn)
  (funcall accessfn (cdr (assoc det *quantifier-polarities*))))
(defun positive-restrictor-det? (det)
  (eql (det-polarity-accessor det #'first) '+))
(defun negative-restrictor-det? (det)
  (eql (det-polarity-accessor det #'first) '-))
(defun positive-body-det? (det)
  (eql (det-polarity-accessor det #'second) '+))
(defun negative-body-det? (det)
  (eql (det-polarity-accessor det #'second) '-))

;; "Quantifiers where the minimal extension of the intersection of the two
;; predicates is non-zero.
(defparameter *non-zero-min-quantifiers*
  '(each.d some.d a.d an.d several.d few.d many.d most.d the.d a_few.d both.d))
(defun non-zero-min-quantifier? (det)
  (member det *non-zero-min-quantifiers*))
;; Quantifiers where the maximal extension of the intersection of the two
;; predicates is zero.
(defparameter *zero-max-quantifiers*
  '(none.d no.d neither.d))
(defun zero-max-quantifier? (det)
  (member det *zero-max-quantifiers*))

(defparameter *number-name-alist*
  (loop for i to 999999 collect (cons (format () "~r" i) i)))

(defun det-number-value! (det)
  (cdr (assoc (car (split (write-to-string det) ".")) *number-name-alist* :test 'equalp)))

(defparameter *quantifier-number-equivalences*
  '((a.d . 1)
    (an.d . 1)
    (both.d . 2)))
(defun number-quantifier? (det)
  (or (integerp (cdrassoc det *quantifier-number-equivalences*))
      (integerp (det-number-value! det))))

(defun flip-existential-there-predicate-plurality! (pred)
  "Flips the plurality of the predicate and the indefinite quantifier
  accordingly."
  (let* ((np (second (second pred)))
         (hn (ulf:find-np-head np :callpkg :ulf-fracas))
         (new-hn (if (listp hn) (second hn) (list 'plur hn)))
         (new-np (ulf:replace-np-head np new-hn :callpkg :ulf-fracas)))
    (list '=
          (list (get-copula-indef! new-np)
                new-np))))

(defun flip-plurality-of-existential-there (ulf)
  "Flips the plurality of an existential-there ULF."
  (when (not (ttt:match-expr
               '(there.pro ((lex-tense? be.v) (? not)
                                              (= (copula-indef? _!))))
               ulf))
    ; wrong form, return nil
    (return-from flip-plurality-of-existential-there nil))
  (ttt:apply-rule
    '(/ (there.pro ((lex-tense? be.v) (? not) _!))
        (there.pro ((lex-tense? be.v)
                    ?
                    (flip-existential-there-predicate-plurality! _!))))
    ulf :shallow t :max-n 1))

(defun fix-quantifier-plurality-agreement (ulf)
  (let ((det
          (ttt:apply-rule '(/ ((number-quantifier? pred?) _!)
                               number-quantifier?)
                          ulf :shallow t :max-n 1)))
    (if (eq (cdrassoc det *quantifier-number-equivalences*) 1)
      (ttt:apply-rule
        '(/ (number-quantifier? (plur noun?))
            (number-quantifier? noun?))
        ulf))))

(defun select-number-det! (d)
  (read-from-string (concatenate 'string (format () "~r" d) ".d")))

(defun minus-one! (x)
  (- x 1))

(defun plus-one! (x)
  (+ x 1))


(defun equivalent-at-least-inferences (ulf)
  (when (not (ttt:match-expr
               '(((nquan (at_least.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
               ulf))
    ; wrong form, return nil
    (return-from equivalent-at-least-inferences nil))
  (let* ((det
          (ttt:apply-rule '(/ (((nquan (at_least.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
                              number-quantifier?)
                          ulf :shallow t :max-n 1))
         (number-det (det-number-value! det))
         (result (ttt:apply-rule '(/ (((nquan (at_least.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
                                     (((select-number-det! (det-number-value! number-quantifier?)) pred?) _*1 individual-level-vp? _*2))
                                 ulf))
         (result-list (list result)))
    (loop for i in '(1 2 3 4 5)
      do
        (progn
          (setf result (ttt:apply-rule '(/ ((number-quantifier? pred?) _*1 individual-level-vp? _*2)
                                           (((select-number-det! (plus-one! (det-number-value! number-quantifier?))) pred?) _*1 individual-level-vp? _*2))
                                        result :shallow t :max-n 1))
          (setf result-list (cons result result-list))))
    result-list))

(defun substitute-number-det! (det)
  (select-number-det! (cdrassoc det *quantifier-number-equivalences*)))
(defun equivalent-number-quantifier? (det)
  (and (number-quantifier? det)
       (assoc det *quantifier-number-equivalences*)))

(defun equivalent-number-substitution-inferences (ulf)
  (let ((result
          (ttt:apply-rule '(/ ((equivalent-number-quantifier? pred?) _*1 individual-level-vp? _*2)
                              (((substitute-number-det! equivalent-number-quantifier?) pred?) _*1 individual-level-vp? _*2))
                          ulf :shallow t :max-n 1)))
    (if result
      (list result)
      nil)))

(defun equivalent-at-most-inferences (ulf)
  (when (not (ttt:match-expr
               '(((nquan (at_most.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
               ulf))
    ; wrong form, return nil
    (return-from equivalent-at-most-inferences nil))
  (let* ((det
          (ttt:apply-rule '(/ (((nquan (at_most.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
                              number-quantifier?)
                          ulf :shallow t :max-n 1))
         (number-det (det-number-value! det))
         (result (ttt:apply-rule '(/ (((nquan (at_most.mod-a number-quantifier?)) pred?) _*1 individual-level-vp? _*2)
                                     (((select-number-det! (det-number-value! number-quantifier?)) pred?) _*1 individual-level-vp? _*2))
                                 ulf))
         (result-list (list result)))
    (loop for i in '(1 2 3 4 5)
      while (>= number-det 1)
      do
        (progn
          (setf result (ttt:apply-rule '(/ ((number-quantifier? pred?) _*1 individual-level-vp? _*2)
                                           (((select-number-det! (minus-one! (det-number-value! number-quantifier?))) pred?) _*1 individual-level-vp? _*2))
                                        result :shallow t :max-n 1))
          (setf number-det (minus-one! number-det))
          (setf result-list (cons result result-list))))
    result-list))
      

(defun select-indefinite! (pred)
  "Select the indefinite for a ULF predicate by using the first surface token."
  ;; This might be good to add to ULF-LIB, or more generally fix-indefinite-selection
  (add-suffix
    (indefinite-article
      (nth-value 0
                 (split-by-suffix
                   (find-first-leaf pred #'ulf:surface-token?))))
    'd :pkg :ulf-fracas))

(defun fix-indefinite-selection (ulf)
  "For each indefinite in the ULF, ensures that the correct one is chosen."
  (ttt:apply-rule
    '(/ ((! a.d an.d) pred?)
        ((select-indefinite! pred?) pred?))
    ulf))

(defun individual-level-vp? (vp)
  "Returns whether this ULF VP is an individual-level predication."
  (or (verb? vp) (tensed-verb? vp)))

(defun equivalent-quantifier-substitution-inferences (ulf)
  "Inferences where equivalent quantifiers are substituted for each other while
  preserving naturality (e.g plurality)"
  (let ((exist-there-plur-flip-result
          (flip-plurality-of-existential-there ulf)))
    ;; Wrap as a list.
    (setf exist-there-plur-flip-result
          (if exist-there-plur-flip-result
            (list exist-there-plur-flip-result)
            nil))
    
    (remove-if #'null
               (append
                 ;; Call all functions, then clean up the indefinites and plurality agreement.
                 (mapcar
                   (compose #'fix-indefinite-selection)
                   ;; Get all raw results in a big list.
                   (append
                     exist-there-plur-flip-result
                     ;; a/an<->some
                     (ttt-apply-rule-possibilities '(/ a.d some.d) ulf :max-per-tree 1 :min-per-tree 1)
                     (ttt-apply-rule-possibilities '(/ some.d a.d) ulf :max-per-tree 1 :min-per-tree 1)
                     ;; all<->every
                     (ttt-apply-rule-possibilities '(/ all.d every.d) ulf :max-per-tree 1 :min-per-tree 1)
                     (ttt-apply-rule-possibilities '(/ every.d all.d) ulf :max-per-tree 1 :min-per-tree 1)
                     ;; kind<->all for individual-level predicates
                     (ttt-apply-rule-possibilities '(/ ((k pred?) _*1 individual-level-vp? _*2)
                                                       ((all.d pred?) _*1 individual-level-vp? _*2))
                                                   ulf :max-per-tree 1 :min-per-tree 1)
                     (ttt-apply-rule-possibilities '(/ ((all.d pred?) _*1 individual-level-vp? _*2)
                                                       ((k pred?) _*1 individual-level-vp? _*2))
                                                   ulf :max-per-tree 1 :min-per-tree 1)))
                 ; number equivalences
                 (equivalent-at-most-inferences ulf)
                 (equivalent-at-least-inferences ulf)
                 (equivalent-number-substitution-inferences ulf)))))

(defun equivalent-quantifier-substitution-inferences-marking-from-source-dispatch (ulf src-ulfs)
  "Dispatch function for getting polarity markings for a ULF from equivalent-quantifier-substitution-inferences."
  ;; 1. Find single point of divergence
  ;; 2. Map all the same polarities except for divergence, where we just propagate it down.
  (assert (= 1 (length src-ulfs)))
  (labels
    ((apply-polarity-with-divergence-point (curlulf
                                            srcmulf
                                            divergence-points
                                            in-divergence
                                            divergence-polarity)
       "Applies the polarity from the source to the current ULF, while
       accounting for divergence. Divergence polarity is simply propagated
       down."
       (let ((curpol (if in-divergence
                       divergence-polarity
                       (global-polarity srcmulf))))
         (cond
           ;; Base case, doesn't matter if we're divergent.
           ;; This is able to handle plurality deletion.
           ((atom (expr curlulf))
            (make-instance 'labeled-tree :expr (expr curlulf) :label curpol))
           ;; At divergence point.
           ;; Recurse with current polarity and ignoring srcmulf.
           ((member (label curlulf) divergence-points)
            (make-instance 'labeled-tree
                           :expr (mapcar #'(lambda (child)
                                             (apply-polarity-with-divergence-point
                                               child nil divergence-points t curpol))
                                         (expr curlulf))
                           :label curpol))
           ;; In a divergence and non-atomic curlulf, recurse while ignoring srcmulf.
           ;; This handles plurality introduction.
           (in-divergence
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (curchild)
                                (apply-polarity-with-divergence-point
                                  curchild
                                  nil
                                  divergence-points
                                  in-divergence
                                  curpol))
                            (expr curlulf))
              :label curpol))
           ;; Not at divergence point.
           ;; Recurse normally.
           (t
            (make-instance
              'labeled-tree
              :expr (mapcar #'(lambda (curchild srchild)
                                (apply-polarity-with-divergence-point
                                  curchild srchild divergence-points nil nil))
                            (expr curlulf)
                            (get-ulf srcmulf))
              :label curpol))))))
    (let* ((src-mulf (get-infer-nlog-marked-ulf (first src-ulfs)))
           (reslulf (dfs-label-tree ulf))
           (srclulf (dfs-label-tree (first src-ulfs)))
           (divergences (find-divergences reslulf srclulf)))
      ;; Not a valid assertion since we can get divergences at plural operator introduction/deletion.
      ;(assert (= 1 (length divergences)))
      (labeled-tree-to-marked-ulf
        (apply-polarity-with-divergence-point
          reslulf
          src-mulf
          (mapcar #'car divergences)
          nil
          nil)))))
;; Register this function for polarity marking dispatch.
(setf (gethash 'equivalent-quantifier-substitution-inferences
               *infer-nlog-marking-dispatch-fn-ht*)
      #'equivalent-quantifier-substitution-inferences-marking-from-source-dispatch)

