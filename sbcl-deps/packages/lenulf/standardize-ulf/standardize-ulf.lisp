;;; Gene Kim, 2020-10-16
;;;
;;; Standardize ULFs parsed by Len's ULF parser to the annotation standard.
;;; This will enable better type analysis and improve interface with other ULF
;;; tools.

(in-package :standardize-ulf)

(defun cdrassoc (x y) (cdr (assoc x y)))

(defun remove-token-index (idxsym &key (delim #\~))
  "Removes the indexing from a symbol token, assuming the indexing is provided
  with a delimiter at the end of the symbol. Default delimiter, tilde."
  (let ((symstr (symbol-name idxsym)))
    (intern (subseq symstr 0 (position delim symstr :from-end t))
            :standardize-ulf)))

(defun remove-token-indices (idxulf &key (delim #\~))
  "Removes the indexings from a ulf, assuming the indexing is provided with a
  delimiter at the end of the symbol. Default delimiter, tilde."
  (cond
    ((null idxulf) idxulf)
    ((atom idxulf) (remove-token-index idxulf :delim delim))
    (t (mapcar #'(lambda (x) (remove-token-indices x :delim delim))
               idxulf))))

;; TODO: move this to utilities
(defun count-elems (elements &key (test #'equal))
  (let ((frequencies (make-hash-table :test test))
        (bags (make-hash-table :test #'eql)))
    (dolist (e elements) (incf (gethash e frequencies 0)))
    (maphash (lambda (k v) (push k (gethash v bags))) frequencies)
    (values bags frequencies)))

(defun lemmatize-len-aux! (aux)
  "Lemmatizes surface-form auxiliary symbols using the pattern.en library and
  adds the appropriate tense. 'be' and 'have' are converted to verbs and other
  auxiliaries are simply mapped to aux-v since this is a stronger assumption.
  If tense is already available, just use that.

  The function assumes that the input, `aux` is either a symbol or a pair of
  tense and aux symbols.

  e.g.
    is.aux -> (pres be.v)
    was.aux -> (past be.v)
    have.aux -> (pres have.v)
    had.aux -> (past have.v)
    did.aux -> (past do.aux-v)
    (pres can.aux) -> (pres can.aux-v)
    |'s.aux| -> (pres be.v)
  "
  ;; No change if the format doesn't match the expected format.
  (when (not (or (len-aux? aux)
                 (and (listp aux) (= (length aux) 2)
                      (lex-tense? (first aux))
                      (len-aux? (second aux)))))
    (return-from lemmatize-len-aux! aux))

  (labels
    ((get-tense (wrd)
       (let* (;; Returns a list of lists where the inner list is the full conjugation info.
              (tenses (python-eval
                        (let ((*package* (find-package :standardize-ulf)))
                          (format nil "list(tenses(\"~s\"))" wrd))))
              ;; Pull out tenses, first element, and count.
              (tense-counts (count-elems (mapcar #'first (coerce tenses 'list))))
              ;; Get highest count.
              (max-tense-count (apply #'max (alexandria:hash-table-keys tense-counts)))
              ;; Get first corresponding tense.
              (mode-tense (let ((*package* (find-package :standardize-ulf)))
                            (read-from-string
                              (first (gethash max-tense-count tense-counts)))))
              ;; Convert tense to ULF format.
              (ulf-tense (cdrassoc mode-tense '((present . pres)
                                                (infinitive . pres)
                                                (past . past)))))
         ulf-tense))) ; end of labels defs
    (let*
      ((sym (if (atom aux) aux (second aux)))
       (sympair (multiple-value-list (split-by-suffix sym)))
       (wrd (first sympair))
       (lemma
         (cond
           ((member wrd '(|'s| |'S|)) 'be)
           (t
            (let ((*package* (find-package :standardize-ulf)))
              (read-from-string
                (python-eval
                  (format nil "str(lemma(\"~s\"))" wrd)))))))
       ;; Tense
       (ulf-tense (if (atom aux) (get-tense wrd) (first aux)))
       ;; Determine suffix.
       (ulf-suffix (case lemma
                     (be 'v)
                     (have 'v)
                     (otherwise 'aux-v))))

      ;; Build.
      (list ulf-tense (add-suffix lemma ulf-suffix)))))

(defun gerundify! (ulf-atom)
  "Takes a ULF atom and returns the same but with the symbol as a gerund.
  Assumes the atom is verb-like. Example:
    run.v -> running.v
    glide.v -> gliding.v
    lead.v -> leading.v"
  (multiple-value-bind (wrd suffix) (split-by-suffix ulf-atom)
    (let ((conjugated
            (python-eval
              (let ((*package* (find-package :standardize-ulf)))
                (format nil "str(conjugate(\"~s\", aspect=PROGRESSIVE))" wrd)))))
      (add-suffix (intern (string-upcase conjugated) :standardize-ulf) suffix))))

;;
;; Len's parser-specific suffix matching.
;;
(defun len-aux? (inx)
  (in-intern (inx x :standardize-ulf)
    (equal 'aux (nth-value 1 (split-by-suffix x)))))

(defun len-adv? (inx)
  (in-intern (inx x :standardize-ulf)
    (equal 'adv (nth-value 1 (split-by-suffix x)))))

(defun prt? (inx)
  (in-intern (inx x :standardize-ulf)
    (equal 'prt (nth-value 1 (split-by-suffix x)))))

(defun replace-suffix! (sym new-suffix)
  (multiple-value-bind (lemma _) (split-by-suffix sym)
    (declare (ignore _))
    (add-suffix lemma new-suffix)))

(defun rel-pro? (sym)
  "Returns t if the symbol is a possible relativizer with a pronoun suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'pro)
         (member lemma '(who that tht which whom what when where whose)))))

(defparameter *determiners*
  ; TODO: look at wiktionary data to get full list.
  '(all every some a an many most much few several the no that which my her his your their one neither both))

(defun adj-det? (sym)
  "Returns t if the symbol is a possible determiner with an adjective suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'a)
         (member lemma *determiners*))))
(defun pro-det? (sym)
  "Returns t if the symbol is a possible determiner with a pronoun suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'pro)
         (member lemma *determiners*))))
(defun unknown-det? (sym)
  "Returns t if the symbol is a possible determiner with no known type."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (lex-unknown? sym)
         (member lemma *determiners*))))

(defun bad-a-few-joined? (expr)
  "Matches '(A.* (FEW.* ...))"
  (and (listp expr)
       (= 2 (length expr))
       (symbolp (first expr))
       (eql 'a (nth-value 0 (ulf:split-by-suffix (first expr))))
       (listp (second expr))
       (symbolp (first (second expr)))
       (eql 'few (nth-value 0 (ulf:split-by-suffix (first (second expr)))))))
(defun fix-a-few-joined! (expr)
  (cons
    (fix-a-few! (first expr) ; a
                (first (second expr))) ; few
    (cdr (second expr)))) ; predicate
(defun fix-a-few! (aexpr fewexpr)
  (let* ((asym (ulf:split-by-suffix aexpr))
         (fewsym (ulf:split-by-suffix fewexpr)))
    (ulf:add-suffix
                    (fuse-into-atom (list asym '_ fewsym) :pkg :standardize-ulf)
                    'd
                    :pkg :standardize-ulf)))

(defparameter *a-few-joined-fix*
  '(/ bad-a-few-joined? (fix-a-few-joined! bad-a-few-joined?)))
(defparameter *a-few-flat-fix1*
  '(/ (_+1 a-x? few-x? _*2)
      (_+1 (fix-a-few! a-x? few-x?) _*2)))
(defparameter *a-few-flat-fix2*
  '(/ (_*1 a-x? few-x? _+2)
      (_*1 (fix-a-few! a-x? few-x?) _+2)))
(defparameter *a-few-flat-fix3*
  '(/ (a-x? few-x?)
      (fix-a-few! a-x? few-x?)))

(defun remove-vp-tense! (vp)
  "Removes the tense from the head verb of the ULF verb phrase."
  (let ((vphead (find-vp-head vp :callpkg :standardize-ulf)))
    (cond
      ((atom vphead) vp) ; already not tensed
      ((and (listp vphead) (= 2 (length vphead)) (lex-tense? (first vphead)))
       (replace-vp-head vp (second vphead) :callpkg :standardize-ulf))
      (t
        (error "Unknown VP head structure: ~s~%    Source VP: ~s~%" vphead vp)))))

(defun tensed-vp? (vp)
  (not (atom (find-vp-head vp :callpkg :standardize-ulf))))

(defun lex-to-noun! (lex-adj)
  "Converts a lexical ULF item to a noun."
  (add-suffix (split-by-suffix lex-adj) 'n))

(defun lex-unknown? (term)
  "Returns whether the given term is a lexical and of an unknown type."
  (and (atom term)
       (not (len-aux? term))
       (not (len-adv? term))
       (let ((types (ulf:phrasal-ulf-type? term :callpkg :standardize-ulf)))
         (equal types '(unknown)))))

(defun add-bars! (term)
  "Add bars around a symbol by adding a space before intern."
  ;(format t "add-bars!: ~s~%" term)
  (intern
    (concatenate 'string " " (symbol-name term))
    :standardize-ulf))

(defun merge-lex-names! (names)
  (intern (apply 'concatenate
                 (cons 'string (mapcar #'symbol-name names)))
          :standardize-ulf))

(defun convert-expr-to-type (expr suffix)
  (cond
    ;; Swap out the suffix.
    ((or (symbolp expr) (numberp expr))
     (ulf:add-suffix
       (nth-value 0 (ulf:split-by-suffix expr))
       suffix
       :pkg :standardize-ulf))
    ;; Give up.
    (t expr)))

(defun nominalize-ulf-expr! (expr)
  "Simple inference to nominalize. For symbols, simply swap the suffix.
  Otherwise, give up for now."
  (convert-expr-to-type expr 'n))
(defun adjectivize-ulf-expr! (expr)
  (convert-expr-to-type expr 'a))

(defparameter *english-relativizers*
  '(who whom which that whose where when what))

(defun possible-relativizer? (ulf)
  "A pronoun or non-pronoun which may be a mis-identified relativizer."
  (when (atom ulf)
       (let ((wrd (split-by-suffix ulf)))
                (member wrd *english-relativizers*))))

;; Checks each part of the ULF if any parts without the suffix are members of
;; the *english-relativizers* then if they are switch their suffix to be a
;; pronoun
(defun switch-simple-possible-rel-to-pro (ulf)
    (cond
       ((and (atom ulf)
             (member (split-by-suffix ulf) *english-relativizers*))
        (convert-expr-to-type ulf 'pro))
       ((and (listp ulf)
             (member (split-by-suffix (car ulf)) *english-relativizers*))
        (cons (convert-expr-to-type (car ulf) 'pro)
              (cdr ulf)))
       ((and (listp ulf)
             (member (split-by-suffix (second ulf)) *english-relativizers*)
             (eql (car ulf) 'sub))
        (cons (first ulf)
              (cons (convert-expr-to-type (second ulf) 'pro)
                    (cddr ulf))))
       ((or (atom ulf) (listp ulf)) ulf)))

(defun switch-possible-rel-to-pro (ulf)
  (let ((simple-res (switch-simple-possible-rel-to-pro ulf))
        (complex-res (cons (switch-simple-possible-rel-to-pro (first ulf))
                           (cdr ulf))))
    (if (not (equal simple-res ulf))
      simple-res
      complex-res)))

;; Only checks for cases without modifiers around the sentence.
(defun possible-simple-relative-clause? (ulf)
  (ttt::match-expr '(! (possible-relativizer? _+)
                       (sub possible-relativizer? _!))
                   ulf))

(defun possible-relative-clause? (ulf)
  "A tensed sentence which may be a relative clause is one which starts with a
  pronoun or a substitution of a pronoun which may be a relativizer: who, whom,
  which, that, tht, etc."
  (if (or (possible-simple-relative-clause? ulf)
          (ttt:match-expr '(possible-simple-relative-clause? (+ sent-mod?))
                          ulf))
    (tensed-sent? (switch-possible-rel-to-pro ulf))
    nil))

(defun relativize-sent! (ulf)
  "Takes a sentence that might be a relative clause with the relativizer
  mislabeled as a pronoun and converts it to an actual relative clause.
  Assumes the input is a tensed sentence with a subject or substituted
  pronoun."
  (cond
    ((eql 'sub (first ulf))
     (cons 'sub (relativize-sent! (cdr ulf))))
    (t (cons (convert-expr-to-type (car ulf) 'rel)
             (cdr ulf)))))

(defun flat-sent? (ulf)
  "Whether this ULF is a flat sentence construction that is common in some
  parsers. This is recognized through a TERM VP next to each other and
  bracketed with other things with known types, but is analyzed as an unknown
  type."
  (and (or (ttt:match-expr '(_+ term? (* len-adv? sent-mod?)
                                (! verb? tensed-verb?) _*)
                           ulf)
           (ttt:match-expr '(_* term? (* len-adv? sent-mod?)
                                (! verb? tensed-verb?) _+)
                           ulf))
       (ulf::unknown? ulf)
       (not (some #'ulf::unknown? ulf))))

(defun fix-flat-sent! (ulf)
  "Mapping function corresponding to flat-sent?. Assumes that flat-sent? is
  true already. Simply brackets the term and verb together."
  (ttt:apply-rules
    '((/ (_+1 term? (*2 len-adv? sent-mod?) (!3 verb? tensed-verb?) _*4)
         (_+1 (term? *2 !3) _*4))
      (/ (_*1 term? (*2 len-adv? sent-mod?) (!3 verb? tensed-verb?) _+4)
         (_*1 (term? *2 !3) _+4)))
    ulf))

;; Removes periods from ULFs.
(defparameter *ttt-remove-periods*
    '(/ (_*1 (! \. (\.)) _*2)
       (_*1 _*2)))

;; Removes double parenthesis.
(defparameter *ttt-remove-parenthesis*
    '(/ ((_+)) (_+)))

;; Removes all punctuations.
(defparameter *ttt-remove-punctuations*
  '(/ (_*1 punct? _*2)
      (_*1 _*2)))

(defparameter *punct-list*
    '(\: \' \. \, \- \_ \{ \} \[ \] \~
      \; ;; The semicolon was messing up the editor highlighting
         ;; so it was put in this form
     ))

(defun punct? (x)
  (in-intern (x new-x :standardize-ulf) (if (member new-x *punct-list*) t nil)))

(defun tree-contains? (tree pred)
  (cond
    ((funcall pred tree) t)
    ((atom tree) nil)
    (t (some #'(lambda (x) (eql x t))
             (mapcar #'(lambda (sub) (tree-contains? sub pred)) tree)))))

(defun cc-mismatched-types? (ulf)
  "Identifies coordinated conjunctions of ULFs with mismatched types."
  (and (listp ulf)
       ;; contains a coordinator
       (not (null (remove-if-not #'lex-coord? ulf)))
       ;; ensure unknowns are already converted to names
       (not (tree-contains? ulf #'lex-unknown?))
       (let ((args (remove-if #'lex-coord? ulf)))
         ;; at least 1 arg
         (and (> (length args) 1)
              ;; no agreed type
              (null (reduce #'intersection
                            (mapcar #'phrasal-ulf-type? args)))))))

(defun cdr-max (cons1 cons2)
  (if (> (cdr cons1) (cdr cons2))
    cons1
    cons2))

(defun enforce-cc-types! (ulf)
  "Enforce the majority type on coordinated conjunctions."
  (let*
    ((types (mapcar #'(lambda (expr)
                        (phrasal-ulf-type? expr :callpkg :standardize-ulf))
                    ulf))
     (type-counts
       (reduce
         #'(lambda (count-alist newtypes)
             (loop for ntype in newtypes do
                   (if (assoc ntype count-alist)
                     ;; increment
                     (rplacd (assoc ntype count-alist)
                             (1+ (cdr (assoc ntype count-alist))))
                     ;; add new
                     (setf count-alist (acons ntype 1 count-alist))))
             count-alist)
         types
         :initial-value nil))
     (majority-type-cons
       (reduce #'cdr-max
               (remove-if #'(lambda (pair) (eql 'coordinator (car pair)))
                          type-counts)))
     (majority-type (car majority-type-cons)))

    ;; Enforce the majority type for non-coordinators.
    (mapcar
      #'(lambda (expr expr-types)
          (cond
            ((member 'coordinator expr-types)
             expr)
            ((member majority-type expr-types)
             expr)
            (t
              (convert-to-type expr majority-type))))
      ulf
      types)))

;; TODO: move to ulf-lib
(defparameter *basic-types-to-suffix*
  '((noun . n)
    (adj . a)
    (p-arg . p-arg)
    (prep . p)
    (verb . v)
    (aux . aux-v)))

(defun convert-to-type (ulf new-type)
  "Convert ulf to have new-type."
  (cond
    ;; Atoms turn to names.
    ((and (atom ulf) (equal 'term new-type))
     (add-bars! (split-by-suffix ulf)))
    ;; Adjectives.
    ((and (atom ulf) (assoc new-type *basic-types-to-suffix*))
     (replace-suffix! ulf (cdr (assoc new-type *basic-types-to-suffix*))))
    ;; Unhandled cases, raise error.
    (t (error
         (format nil
                 "Unknown type conversion.~%  ulf: ~s~%  new-type: ~s~%~%"
                 ulf new-type)))))

(defun det2adj! (det)
  (if (atom det)
    (replace-suffix! det 'a)
    (second det)))

(defun det2adv-s! (det)
  (if (atom det)
    (replace-suffix! det 'adv-s)
    (list 'adv-s (second det))))

(defun at-x? (ulf)
  (and (atom ulf)
       (eql 'at (split-by-suffix ulf))))
(defun at-most/least? (ulf)
  (and (atom ulf)
       (member (split-by-suffix ulf) '(least most))))
(defun a-x? (ulf)
  (and (atom ulf)
       (eql 'a (split-by-suffix ulf))))
(defun few-x? (ulf)
  (and (atom ulf)
       (eql 'few (split-by-suffix ulf))))
(defun merge-symbols! (x y suf)
  (ulf:add-suffix
    (fuse-into-atom (list (split-by-suffix x)
                          '_
                          (split-by-suffix y))
                    :pkg :standardize-ulf)
    suf
    :pkg :standardize-ulf))

;; Fixing the possessives form

;; n+preds singular pred case
(defparameter *ttt-fix-possessives-sing*
  '(/ ((det? (n+preds _!1 |'S|)) _!2)
    (((det? _!1) 's) _!2)))

;; n+preds plural preds case
(defparameter *ttt-fix-possessives-pl*
  '(/ ((det? (n+preds _!1 _+ |'S|)) _!2)
    (((det? (n+preds _!1 _+)) 's) _!2)))

;; Rules used for performing domain-specific fixes.
(defparameter *ttt-ulf-fixes*
  (list
    ;'(/ ((lex-tense? have.v) _*1 ((perf lex-verb?) _*2)) ((lex-tense? perf) _*1 (lex-verb? _*2)))

    ;; Weird possessives bug for words ending in "s".
    '(/ |foot2|
        (quote s))

    ;; Removing periods from ULFs.
    *ttt-remove-periods*

    ;; Removing double parenthesis from ULFs.
    *ttt-remove-parenthesis*


    ;; Lift question marks and exclamation marks to scope around sentence.
    '(/ (_!1 _+2 (!3 [!] [?]))
        ((_!1 _+2) !3))

    ;; Removing periods from ULFs.
    *ttt-remove-periods*

    ;; Removing double parenthesis from ULFs.
    *ttt-remove-parenthesis*

    ;;
    ;; Fix punctuation
    ;;
    *ttt-remove-punctuations*

    ;; N+PREDS bug.
    '(/ n+pred n+preds)
    ;; Negation
    '(/ (! not.adv |N'T.ADV|) not)
    ;; Stray PRT
    '(/ prt? (replace-suffix! prt? adv-a))
    ;; Add tense, lemmatize, and canonicalize suffix for auxiliaries.
    '(/ (! (lex-tense? len-aux?))
        (lemmatize-len-aux! !))
    '(/ (! (_*1 len-aux? _*2) ~ (lex-tense? len-aux?))
        (_*1 (lemmatize-len-aux! len-aux?) _*2))

    ;; Remove tense under ka/to operators.
    '(/ ((!1 to ka) tensed-vp?)
        (!1 (remove-vp-tense! tensed-vp?)))

    ;; Fix head-less determiners.
    '(/ (det? adj?)
        (det? (adj? {ref}.n)))

    ;; Convert adjective modifiers of implicit nouns to nouns.
    '(/ (lex-adjective? {ref}.n)
        (lex-to-noun! lex-adjective?))

    ;; Modifiers.
    ;; (ADV A) -> (MOD-A A)
    '(/ (len-adv? (! adj? pp?))
        ((replace-suffix! len-adv? mod-a)
         !))
    ;; (ADV N) -> (MOD-N N)
    '(/ (len-adv? noun?)
        ((replace-suffix! len-adv? mod-n)
         noun?))

    ;; Quantifiers
    ;; (K (QUANT.A ...)) -> (QUANT.D ...)
    '(/ (k (adj-det? (! noun? pp?)))
        ((replace-suffix! adj-det? d)
         !))
    ;; (QUANT.PRO N) -> (QUANT.D N)
    '(/ (pro-det? (! noun? pp?))
        ((replace-suffix! pro-det? d)
         !))
    ;; (QUANT[NO SUFFIX] ...) -> (QUANT.D ..)
    '(/ (unknown-det? (! noun? pp?))
        ((replace-suffix! unknown-det? d)
         !))
    ;; (A.* (FEW.* ...)) -> (a_few.d ...)
    *a-few-joined-fix*
    ;; (... A.* FEW.* ...)) -> (... a_few.d ...)
    *a-few-flat-fix1*
    *a-few-flat-fix2*
    *a-few-flat-fix3*
    ;; (at.x most.x/least.x quant.x)
    ;; -> (nquan (at_most.mod-a quant.a))
    '(/ (at-x? at-most/least? (! adj-det? pro-det? det? unknown-det?))
        (nquan ((merge-symbols! at-x? at-most/least? mod-a)
                (replace-suffix! ! a))))
    '(/ ((at-x? at-most/least?) (! adj-det? pro-det? det? unknown-det?))
        (nquan ((merge-symbols! at-x? at-most/least? mod-a)
                (replace-suffix! ! a))))

    ;; Double determiners
    ;; all the men
    ;; -> (all.d (the.d (plur man.n)))
    ;; -> (all.d ({of}.p (the.d (plur man.n))))
    '(/ ((!1 det?) ((!2 det?) (!3 noun? pp?)))
        (!1 ({of}.p (!2 !3))))

    ;; Modified quantifiers
    '(/ (len-adv? det?)
        ;; todo: infer whether it's nquan or fquan.
        (nquan ((replace-suffix! len-adv? mod-a) ; todo: infer if mod-a or adv-s
                (det2adj! det?))))

    ;; Introduce N+PREDS
    ;; ((k/Q X) PRED) -> (k/Q (N+PREDS X PRED))
    '(/ (((!1 det? k) (!2 ~ (n+preds _*))) (!3 pred? ~ verb? tensed-verb?))
        (!1 (n+preds !2 !3)))

    ;; Fixing the possessives form
    *ttt-fix-possessives-sing*
    *ttt-fix-possessives-pl*

    ;; Fix terms in N+PREDS
    ;; This doesn't work in general since sometimes there are hidden prepositions, e.g.
    ;; (the.d (n+preds right.n (to (live.v (in.p |Europe|))))) should become
    ;; -> (the.d (n+preds right.n ({for}.p (to (live.v (in.p |Europe|))))))
    ;; rather than having the equality.
    '(/ (n+preds _*1 term? _*2) (n+preds _*1 (= term?) _*2))

    ;; (V ... ADV ...) -> (V ... ADV-A ...)
    ;; This makes the stricter assumption about modification, since this can't
    ;; float around. Also, semantically a verb adverbial count be operate at
    ;; sentence-level semantics, but not vice-versa.
    '(/ ((! verb? tensed-verb?) _*1 len-adv? _*2)
        (! _*1
           (replace-suffix! len-adv? adv-a)
           _*2))

    ;; Infer sentence modifiers
    ;; (ADV TERM VP) -> (ADV-S TERM VP)
    ;; (TERM ADV VP) -> (TERM ADV-S VP)
    ;; (TERM VP ADV) -> (TERM VP ADV-S)
    '(/ ((*1 sent-mod?) (!5 len-adv?) (*2 sent-mod? len-adv?) term?
         (*3 sent-mod? len-adv?) (!6 verb? tensed-verb?) (*4 sent-mod? len-adv?))
        (*1 (replace-suffix! !5 adv-s) *2 term? *3 !6 *4))
    '(/ ((*1 sent-mod? len-adv?) term? (*2 sent-mod?) (!5 len-adv?)
         (*3 sent-mod? len-adv?) (!6 verb? tensed-verb?) (*4 sent-mod? len-adv?))
        (*1 term? *2 (replace-suffix! !5 adv-s) *3 !6 *4))
    '(/ ((*1 sent-mod? len-adv?) term?
         (*2 sent-mod? len-adv?) (!6 verb? tensed-verb?)
         (*3 sent-mod?)
         (!5 len-adv?)
         (*4 sent-mod? len-adv?))
        (*1 term? *2 !6 *3 (replace-suffix! !5 adv-s) *4))

    ;; Fix flat TERM-VPs that should be sentences.
    '(/ flat-sent?
        (fix-flat-sent! flat-sent?))

    ;; ((ADV SENT) SENT) -> ((PS SENT) SENT)
    ;; (SENT (ADV SENT)) -> (SENT (PS SENT))
    '(/ ((len-adv? (!1 sent? tensed-sent?)) (!2 sent? tensed-sent?))
        (((replace-suffix! len-adv? ps) !1) !2))
    '(/ ((!1 sent? tensed-sent?) (len-adv? (!2 sent? tensed-sent?)))
        (!1 ((replace-suffix! len-adv? ps) !2)))
    ;; (SENT-MOD TERM VP) -> (SENT-MOD (TERM VP))
    '(/ (sent-mod? term? (! verb? tensed-verb?))
        (sent-mod? (term? !)))
    ;; Weaker version, where in the right context, we assume ADV is PS.
    '(/ ((len-adv? (!1 sent? tensed-sent?))
         (*3 len-adv? sent-mod?) term? (*4 len-adv? sent-mod?)
         (!2 verb tensed-verb?) (*5 len-adv? sent-mod?))
        ((len-adv? !1) (*3 term? *4 !2 *5)))

    ;; Fix unsuffixed (or barred) atoms.
    '(/ lex-unknown? (add-bars! lex-unknown?))

    ;; Merge adjacent names that aren't a part of coordinated terms.
    '(/ (! (_*1 (<> lex-name? (+ lex-name?)) _*2)
           ~ ((! set-of lex-coord?) _* (<> lex-name? (+ lex-name?)) _*)
             (_* (<> lex-name? (+ lex-name?)) _* lex-coord? _+))
        (_*1 (merge-lex-names! (<>)) _*2))

    ;; Correct possessives.
    '(/ (! (| QUOTE S|) (| QUOTE| S))
        's)

    ;; Assume non-leading prepositions are supposed to be adv-a.
    '(/ (_+ lex-prep?)
        (_+ (replace-suffix! lex-prep? adv-a)))

    ;; DETERMINERS
    ;; (<D> x <V>) -> (<D> x <V>)
    '(/ (det? _! tensed-verb?)
        ((det? _!) tensed-verb?))
    ;; (<D> x ...) -> (<D> (x ...))
    '(/ (det? _! _+) (det? (_! _+)))

    ;; RELATIVIZERS
    ;; (n+preds <N> (rel-pro ..)) ->  (n+preds <N> (rel ...))
    '(/ (n+preds noun? _*1 (rel-pro? _+) _*2)
        (n+preds noun?
                 _*1
                 ((replace-suffix! rel-pro? rel) ; replace pro with rel
                  _+)
                 _*2))
    ;; ((k/<D> <N>) (rel-pro ..)) -> (k/<D> (n+preds <N> (rel ..)))
    '(/ (((! k det?) noun?) (rel-pro? _+))
        (! (n+preds noun? ((replace-suffix! rel-pro? rel) _+))))

    ;; ((pres be.v) <term>) -> ((pres be.v) (= <term>))
    ;; (be.v <term>) -> (be.v (= <term>))
    '(/ ((!1 (lex-tense? be.v) be.v) term?)
        (!1 (= term?)))

    ;; PROG in place of modifiers
    ;; NB: This rule is absolutely NOT general. Assumes that we don't have
    ;; tense-less progressives and that these present participle forms always
    ;; turn into adjectives. They can in fact become nouns, verb modifiers, or
    ;; reified verbs.
    '(/ ((prog lex-verb?) noun?)
        ((mod-n (adjectivize-ulf-expr! (gerundify! lex-verb?))) noun?))
    '(/ ((prog lex-verb?) adj?)
        ((mod-a (adjectivize-ulf-expr! (gerundify! lex-verb?))) adj?))


    ;; Likely relativizers
    ;; (N SENT[with possible relativizer pronoun]) -> (N+PREDS N SENT[pro->rel])
    '(/ (noun? possible-relative-clause?)
        (n+preds noun? (relativize-sent! possible-relative-clause?)))
    ;; (quantified-N SENT[with possible relativier pronoun])
    ;; -> (det (n+preds n SENT[pro->rel]))
    '(/ ((det? noun?) possible-relative-clause?)
        (det? (n+preds noun? (relativize-sent! possible-relative-clause?))))
    '(/ (((!1 det?) (lex-p? ((!2 det?) noun?))) possible-relative-clause?)
        (!1 (lex-p? (!2 (n+preds noun? possible-relative-clause?)))))

    ;; Likely kinds
    ;; (NOUN TENSED-VERB) -> ((k NOUN) TENSED-VERB)
    '(/ (noun? tensed-verb?)
        ((k noun?) tensed-verb?))

    ;;
    ;; Fix types
    ;;

    ;; Infer nouns under plur/k operator
    '(/ ((!1 plur k) (!2 ~ noun?))
        (!1 (nominalize-ulf-expr! !2)))
    ;; Infer nouns under determiners (are more permissive than plur/k)
    '(/ (det? (! ~ noun? pp?))
        (det? (nominalize-ulf-expr! !)))

    ;; Fix progressive
    '(/ ((lex-tense? be.v) _* (prog lex-verb?))
        ((lex-tense? prog) _* lex-verb?))
    '(/ ((lex-tense? be.v) _*1 ((prog lex-verb?) _*2))
        ((lex-tense? prog) _*1 (lex-verb? _*2)))

    ;; Fix perfect
    '(/ ((lex-tense? have.v) _* (perf lex-verb?))
        ((lex-tense? perf) _* lex-verb?))
    '(/ ((lex-tense? have.v) _*1 ((perf lex-verb?) _*2))
        ((lex-tense? perf) _*1 (lex-verb? _*2)))

    ;; Fix passive
    ;; '(/ ((lex-tense? be.v) _* (pasv lex-verb?))
    ;;     ((lex-tense? (pasv lex-verb?)) _*))
    '(/ ((lex-tense? be.v) _*1 ((pasv lex-verb?) _*2))
        ((lex-tense? (pasv lex-verb?)) _*1 _*2))

    ;; Reify sentence arguments to verbs.
    ;; NB: sometimes this is actually two arguments with wrong bracketing, but
    ;; we're gonna ignore that for now.
    '(/ ((! verb? tensed-verb?) _*1 tensed-sent? _*2)
        (! _*1 (tht tensed-sent?) _*2))
    '(/ ((! verb? tensed-verb?) _*1 sent? _*2)
        (! _*1 (ke sent?) _*2))

    ;; either-or
    ;; todo: just make the type system more robust to multiple CCs in multiple places.
    '(/ ((! either either.cc either.adv) _*1 (!2 or or.cc) _*3)
        (_*1 either_or.cc _*3))
    '(/ (((! either either.cc either.adv) _!1) (!2 or or.cc) _*3)
        (_!1 either_or.cc _*3))
    '(/ (((! either either.cc either.adv) _+1) (!2 or or.cc) _*3)
        ((_+1) either_or.cc _*3))

    ;; Merge multi-argument copula
    '(/ ((lex-tense? be.v) (det? noun?) pp?)
        ((lex-tense? be.v) (= (det? (n+preds noun? pp?)))))

    ;; Assume that there are no prepositional phrase complements.
    ;; "that" with a sentence turns into a proposition.
    '(/ ((!1 verb? tensed-verb?) _*2 (that.p _!3) _*4)
        (!1 _*2 (that _!3) _*4))
    
    ;; Assume "by" prepositions to passivized verbs are argument markers.
    '(/ ((lex-tense? (pasv verb?)) _*1 (by.p _!) _*2)
        ((lex-tense? (pasv verb?)) _*1 (by.p-arg _!) _*2))

    ;; Assume that there are no prepositional phrase complements.
    ;; Turn them into modifiers.
    '(/ ((! verb? tensed-verb?) _*1 pp? _*2)
        (! _*1 (adv-a pp?) _*2))

    ;; Convert floating determiners to adv-s.
    '(/ (_+ det? _*)
        (_+ (det2adv-s! det?) _*))

    ;; Make types of coordinated conjunctions match.
    '(/ cc-mismatched-types?
        (enforce-cc-types! cc-mismatched-types?))

    ;; Basic it-extraposition.
    '(/ (it.pro ((lex-tense? be.v) adj? term?))
        (it-extra.pro (((lex-tense? be.v) adj?) term?)))
    ))

(defun standardize-ulf (inulf &key pkg)
  "Fixes the parsed ULF with domain-specific fixes which may not generalize
  outside of this package. Assumes the token-indexing has already been
  removed."
  (when (not ulf2english::*setup-complete*)
    (ulf2english::setup-pattern-en-env #'ulf2english::python-over-py4cl))

  (inout-intern (inulf ulf :standardize-ulf :callpkg pkg)
    ;; TODO: make max-n a multiplicative factor of the ulf size
    (ttt:apply-rules *ttt-ulf-fixes* ulf
                     :max-n 1000
                     :deepest t
                     :rule-order :fast-forward)))

