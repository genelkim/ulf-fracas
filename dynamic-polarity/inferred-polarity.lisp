;;; Local polarities that are inferred from a externally marked global
;;; polarity, which is then propagated as possible. This file provides
;;; the functions for managing this system, but does not implement it.

(in-package :ulf-fracas)

;; Hash table from ULF to the polarity operator marking.
(defparameter *polarity-operator-ht* (make-hash-table :test #'equal))
;; Hash table from ULF to the global polarity marking.
(defparameter *global-polarity-ht* (make-hash-table :test #'equal))

(defun scoping-op? (ulf)
  "Predicate whether a ulf expression is a scoping operator."
  ;; 1. determiners
  ;; 2. adv-s
  (or (det? ulf) (adv-s? ulf)))

(defun get-all-scoping-ops (ulf)
  "Extracts all scoping operators from ULF without recursing into constructed
  scoping operators."
  (cond
    ((scoping-op? ulf) (list ulf))
    ((atom ulf) nil)
    ;; Ignore singly-scoped adv-s.
    ((and (= 2 (length ulf)) (scoping-op? (first ulf)))
     (get-all-scoping-ops (second ulf)))
    (t (apply #'append (mapcar #'get-all-scoping-ops ulf)))))

(defun get-scoping-op-labels (lulf)
  "Extracts a list of labels in the labeled ULF corresponding to scoping
  operators.

  Examples:
    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
    -> (2 9)

    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 believe.v])]
                  [8 ([9 that]
                      [10 ([11 ([12 a.d] [13 person.n])]
                           [14 ([15 pres] [16 exist.v])])])])])]
    -> (2 12)"
  (let (curres recres)
    ;; If current node is a scoping operator, set that as the current result.
    (when (scoping-op? (unlabeled-tree lulf))
      (setf curres (list (label lulf))))
    (cond
      ;; Base case - only current result.
      ((atom (expr lulf)) curres)
      ;; Recursive case - add subexpressions to current result.
      (t
       (setf recres (mapcar #'get-scoping-op-labels (expr lulf)))
       (apply #'append (cons curres recres))))))

(defun permutations (lst)
  "All possible permutations of lst"
  (cond
    ((null lst) lst)
    (t (loop for e in lst
             append (mapcar #'(lambda (l) (cons e l)
                                (permutations (remove e lst :test #'equal))))))))

(defun possible-scope-orders (ulf)
  "Produces the possible scoping operator combinations from a ULF."
  ;; Scoping operators are stopped by type-shifters,
  ;; adv-s is forced local if singly paired with an operand.
  (let ((ops (get-all-scoping-ops ulf)))
    (permutations ops)))

(defun lulf-det-rest? (lulf)
  "Returns whether the lulf is a (det rest) construction."
  (and (listp (expr lulf))
       (= 2 (length (expr lulf)))
       (det? (unlabeled-tree (first (expr lulf))))))

(defun lulf2lslf (lulf scoping)
  "Generates a labeled SLF from a labeled ULF using common labels where
  possible. Also returns the determiner information and scoping operator
  information as additional values.

  Example:
    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
    scoping: ((0 . (2 9)))
    -> [nil ([2 every.d] [nil x] [nil ([nil x] [3 man.n])]
             [nil ([9 a.d] [nil y] [10 shirt.n]
                   [0 ([nil x]
                       [4 ([5 ([6 pres] [7 own.v])] [nil y])])])])]"
  ;; 1. Pull the scoping operators out of the lulf on traversal. For
  ;;    determiners pull out the restrictor too and add a placeholder variable.
  ;; 2. Afterwards, re-traverse the formula and add scoping operators
  ;;    right-to-left. This needs to be a separate traversal since
  ;;    sentence-level scoping operators can appear outside of the scoping
  ;;    point when it is already at the correct location.
  (labels
    ((get-scoping-operators (curlulf)
       "Recursively extract scoping operators. Returns
         - An association list from label to non-determiner scoping operators
         - An association list from label to partial determiner info"
       ;; Atomic base cases.
       (cond
         ;; Determiner.
         ;; Generate a variable placeholder, and add the lulf and placeholder
         ;; to partial determiner assoc list. The full determiner information
         ;; requires the restrictor, which is not available at this level.
         ((det? (unlabeled-tree curlulf))
          (let ((var (gensym)))
            (list
              ;; Non-determiner scoping operator assoc list.
              nil
              ;; Partial determiner assoc list.
              (list (cons (label curlulf) var)))))
         ;; Non-determiner scoping operator.
         ((and (atom (expr curlulf))
               (scoping-op? (expr curlulf)))
          (list ;; Non-determiner scoping operator assoc list - from label to operator.
            (list (cons (label curlulf)
                        (expr curlulf)))
            nil))
         ;; Non scoping operator atom.
         ((atom (expr curlulf))
          (list nil nil))

         ;; Recurseive case.
         (t
          ;; Put recurseive results together.
          (let* ((recres (mapcar #'get-scoping-operators (expr curlulf)))
                 (recdets (apply #'append (mapcar #'first recres)))
                 (recsops (apply #'append (mapcar #'second recres)))
                 (det-vars (apply #'append (mapcar #'third recres)))
                 (curulf (unlabeled-tree curlulf)))
            (list recdets recsops det-vars)))))

     (build-det-info (curlulf det-vars)
       "Recursively extracts the determiner information (determiner, variable,
       restrictor) for a labeled (det rest) construction. Calls lulf2lslf to
       get scoped determiner and restrictor representations.

       The lulf2lslf resolves incomplete sop-info so return that as an
       additional value."
       (cond
         ;; At leaf, return empty result.
         ((atom (expr curlulf)) (values nil nil))
         ;; At (det rest), extract info.
         ((lulf-det-rest? curlulf)
          (let* ((recres (mapcar #'(lambda (child)
                                     (multiple-value-list
                                       (lulf2lslf child scoping)))
                                 (expr curlulf)))
                 (lslf-children (mapcar #'first recres))
                 (rec-det-info (apply #'append (mapcar #'second recres)))
                 (rec-sop-info (apply #'append (mapcar #'third recres)))
                 (det-label (label (first (expr curlulf)))))
            ;; Return components.
            (values
              (cons (cons det-label
                        (list
                          ;; 1. determiner
                          (first lslf-children)
                          ;; 2. variable
                          (cdr (assoc det-label det-vars))
                          ;; 3. restrictor
                          (second lslf-children)))
                    rec-det-info)
              rec-sop-info)))
         ;; Recursive and combine.
         (t
          (let ((recres
                  (mapcar #'(lambda (child)
                              (multiple-value-list
                                (build-det-info child det-vars)))
                          (expr curlulf))))
            (values
              (apply #'append (mapcar #'first recres))
              (apply #'append (mapcar #'second recres)))))))

     (build-lslf (curlulf dets sops)
       "Given extracted scoping operators and determiners builds the LSLF."
       (let ((lslf-expr
               (cond
                 ;; Base case.
                 ((atom (expr curlulf))
                  (expr curlulf))
                 ;; Determiner construction ((det rest) body), replace (det
                 ;; rest) with variable since all the processing was completed
                 ;; for copmuting dets.
                 ((lulf-det-rest? curlulf)
                  (second (cdr (assoc (label (first (expr curlulf)))
                                      dets))))
                 ;; General recurse.
                 (t
                  (mapcar #'(lambda (child) (build-lslf child dets sops))
                          (expr curlulf))))))
         (add-scopings-to-node (label curlulf)
                               (reverse (cdr (assoc (label curlulf) scoping)))
                               lslf-expr
                               dets
                               sops)))
     (add-scopings-to-node (curlabel next-scopings slf dets sops)
       "Recursively adds scoping operators to the current label of the SLF
       specified by the `scoping` argument of the top-level function. Returns
       the new LSLF."
       (cond
         ;; Base case - no queued scopings.
         ((null next-scopings)
          (make-instance 'labeled-tree
                         :expr slf
                         :label curlabel))
         ;; Handle the next label queued for scoping at the current position.
         ;; Next label is a determiner.
         ((assoc (car next-scopings) dets)
          ;; Get the determiner info from dets.
          (let*
            ((det-info (cdr (assoc (car next-scopings) dets)))
             (det-lslf (first det-info))
             (var (second det-info))
             (var-lslf (make-instance 'labeled-tree
                                      :expr var
                                      :label nil))
             ;; Restrictor with variable.
             ;; Maybe we'll remove the variable since it's not really
             ;; adding information that cannot be inferred in our case.
             (res-lslf (make-instance 'labeled-tree
                                      :expr (list var-lslf (third det-info))
                                      :label nil))
             (body-lslf (make-instance 'labeled-tree
                                       :expr slf
                                       :label curlabel))
             (new-slf (list det-lslf var-lslf res-lslf body-lslf)))
            ;; Recurse to the next scoping.
            (add-scopings-to-node nil (cdr next-scopings) new-slf dets sops)))
         ;; Next label is a non-determiner scoping operator.
         ((assoc (car next-scopings) sops)
          (let*
            ((sop-lslf (make-instance 'labeled-tree
                                      :expr (cdr (assoc (car next-scopings) sops))
                                      :label (car next-scopings)))
             (body-lslf (make-instance 'labeled-tree
                                       :expr slf
                                       :label curlabel))
             (new-slf (list sop-lslf body-lslf)))
            ;; Recurse into the next scoping.
            (add-scopings-to-node nil (cdr next-scopings) new-slf dets sops)))
         ;; Unknown.
         (t (error "Unknown case for scoping!~%  slf: ~s~%  next-scopings: ~s~%  dets: ~s~%  sops: ~s~%"
                   slf next-scopings dets sops)))))

     (let* ((scoping-op-info (get-scoping-operators lulf))
            (nonrec-sop-info (first scoping-op-info))
            (det-var-info (second scoping-op-info))
            (det+rec-sop-info (multiple-value-list (build-det-info lulf det-var-info)))
            (det-info (first det+rec-sop-info))
            (sop-info (append nonrec-sop-info (second det+rec-sop-info))))
       (values (build-lslf lulf det-info sop-info)
               det-info
               sop-info))))


(defun polarity-for-scoping (lulf scoping)
  "Generates a marked ULF from a labeled ULF and a specified scoping of
  operators.

  Returns three values:
  1. The polarity marked ULF
  2. The index labeled SLF
  3. The polarity marked SLF
  4. The local polarity marking of SLF

  Example:
    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
    scoping: ((0 . (2 9)))
    -> [+ ([+ ([+ every.d] [- man.n])]
           [+ ([+ ([+ pres] [+ own.v])] [+ ([+ a.d] [+ shirt.n])])])]

    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 do.aux-s])]
                  [8 not]
                  [9 ([10 own.v] [11 ([12 a.d] [13 shirt.n])])])])]
    scoping: ((0 . (2 7 8 12)))
    -> [+ ([+ ([+ every.d] [- man.n])]
           [+ ([+ ([+ pres] [+ do.aux-s])]
               [+ not]
               [- ([- own.v] [- ([- a.d] [- shirt.n])])])])]
  "
  ;; 1. Generate a labeled SLF from the scoping.
  ;;   lulf: [0 ([1 ([2 every.d] [3 man.n])]
  ;;             [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
  ;;   scoping: ((0 . (2 9)))
  ;;   -> [nil ([2 every.d] [nil x] [3 man.n]
  ;;            [nil ([9 a.d] [nil y] [10 shirt.n]
  ;;                  [0 ([nil x]
  ;;                      [4 ([5 ([6 pres] [7 own.v])] [nil y])])])])]
  ;;
  ;; 2. Get polarity operations of every node. For determiners, this is a tuple.
  ;;    Label local polarity.
  ;;
  ;; 3. For each numbered node of the labeled SLF, go up tree and count negative
  ;;    polarity operations. We can do this top-down with a DFS traversal.
  ;;
  ;; 4. Use these counts to compute the polarity of the numbered values.
  ;;
  ;; 5. Infer polarity of constituents in the ULF that aren't in the SLF.
  (let*
    (;; 1. Get labeled SLF.
     (lslf (lulf2lslf lulf scoping))
     ;; 2. Get local polarity labeling.
     (local-mslf (mark-local-slf-polarity (unlabeled-tree lslf)))
     ;; 3. Compute global SLF polarity.
     (global-mslf (globalize-slf-polarity local-mslf))
     ;; 4. Use the labeled SLF to map the SLF polarity values to the
     ;; corresponding ULF nodes.
     (partial-mulf (slf-polarities-to-ulf lslf global-mslf lulf))
     ;; 5. Infer the rest of the ulf polarities.
     (complete-mulf (infer-remaining-ulf-polarities partial-mulf)))
    (values complete-mulf lslf global-mslf local-mslf)))

(defun infer-remaining-ulf-polarities (pmulf)
  "Infers remaining polarities in a partial marking of ULF global polarities.

  Inference rules:
  1. If all children share a polarity marking, take that.
  2. Otherwise, take the parent polarity marking.

  This is done in two phases, one bottom-up and one top-down. The second phase
  is to propagate polarities downward to nodes that had no labels for its
  parents or children. Since the root always has a positive polarity, it is
  guaranteed that this will successfuly cover everything in the second phase.

  pmulf:
    A labeled-tree class instance with partially marked polarities.
  Returns:
    A marked-ulf class instance with inferred polarities."
  (labels
    ((bottom-up-phase (curmulf parentpol)
       "Bottom-up phase, which takes the child polarities if agreed, otherwise,
       the parent."
       (let*
         (;; First recurse to get updated children.
          ;; NB: This goes from a labeled-tree class to a marked-ulf class.
          (curexpr (if (atom (expr curmulf))
                     ;; Atom, just use expression.
                     (expr curmulf)
                     ;; List, recurse.
                     (mapcar #'(lambda (child)
                                 (bottom-up-phase child (label curmulf)))
                             (expr curmulf))))
          ;; Update current polarity based on this.
          (curpol
            (cond
              ;; Current label exists.
              ((not (null (label curmulf))) (label curmulf))
              ;; Non-leaf and all children have the same (non-empty) label.
              ((and (listp (expr curmulf))
                    (= 1 (length (remove-duplicates
                                   (mapcar #'global-polarity curexpr))))
                    (not (null (global-polarity (first curexpr)))))
               (global-polarity (first curexpr)))
              ;; Parent has a label
              ((not (null parentpol)) parentpol)
              ;; Otherwise, just nil.
              (t nil))))
         (make-instance 'marked-ulf :ulf curexpr :global-polarity curpol)))
     (top-down-phase (curmulf parentpol)
       "Top-down phase, takes the parent polarity."
       (let ((curpol (if (null (global-polarity curmulf))
                       parentpol
                       (global-polarity curmulf))))
         (make-instance
           'marked-ulf
           :ulf (if (atom (get-ulf curmulf))
                   ;; Atom, just use expression.
                   (get-ulf curmulf)
                   ;; List, recurse.
                   (mapcar #'(lambda (child) (top-down-phase child curpol))
                           (get-ulf curmulf)))
           :global-polarity curpol))))
    ;; Main body of labels.
    (top-down-phase (bottom-up-phase pmulf '+) '+)))

(defun slf-polarities-to-ulf (lslf gmslf lulf)
  "Maps polarity markings from SLFs to ULFs.

  lslf: A node labeled SLF.
  gmslf: An SLF marked with global polarities.
  lulf: A node labeled ULF. The labeling MUST correspond to the labelings in
        lslf. For example, through the lulf2lslf function. Missing
        correspondences are preserved and those polarities may be recovered
        through context."
  ;; 1. Create a mapping from node index to polarity.
  (let ((lpmap (create-label-map lslf gmslf)))
    ;; 2. Apply label mapping to the ULF.
    (apply-label-map lulf lpmap)))

(defun globalize-slf-polarity (local-mslf)
  "Takes a locally polarity-marked SLF and generates a globally polarity-marked
  SLF.

  Performs a single DFS traversal and keeps track of the count of negative and
  blocked polarities.

  Example:
    local-mslf:
      [+ ([nil every.d] [nil x]
          [- ([nil x] [nil man.n])]
          [+ ([nil a.d] [nil y]
              [+ ([nil y] [nil shirt.n])]
              [+ ([nil x] [nil ([nil ([nil pres] [nil own.v])] [nil y])])])])]
    -> [+ ([+ every.d] [+ x]
           [- ([- x] [- man.n])]
           [+ ([+ a.d] [+ y]
               [+ ([+ y] [+ shirt.n])]
               [+ ([+ x] [+ ([+ ([+ pres] [+ own.v])] [+ y])])])])]
  "
  (labels
    ((rechelper (curmslf negcount blocked?)
       "Main recursive helper function which includes relevant polarity counts."
       (let* ((curpol (label curmslf))
              (curnegcount (if (eql '- curpol) (1+ negcount) negcount))
              (curblocked? (or blocked? (eql 'o curpol)))
              ;; Current polarity.
              ;; o if blocked in the path to the root.
              ;; - if there were an odd number of negative local polarities
              ;; + otherwise
              (curpol (cond
                        (curblocked? 'o)
                        ((= (mod curnegcount 2) 1) '-)
                        (t '+)))
              curexpr)
         (cond
           ;; Base case.
           ((atom (expr curmslf))
            (make-instance 'labeled-tree
                           :expr (expr curmslf)
                           :label curpol))
           ;; Recursive case.
           ;; Build the children first.
           (t
            (setf curexpr
                  (mapcar #'(lambda (slf-child)
                              (rechelper slf-child curnegcount curblocked?))
                          (expr curmslf)))
            (make-instance 'labeled-tree
                           :expr curexpr
                           :label curpol))))))
    ;; Main body of labels.
    (rechelper local-mslf 0 nil)))

(defun terms-for-vars (slf vars)
  "Replaces vars with some term, (e.g., it.pro) so that type analysis
  succeeds more often."
  ;; NB: For a completely robust system, this should be replaced by an
  ;; SLF-specific type system which accounts for scoped variables, as well as
  ;; scoped determiners.
  (let ((newslf slf))
    (loop for var in vars do
          (setf newslf (subst 'it.pro var newslf)))
    newslf))

(defun mark-local-slf-polarity (slf)
  "Generates a labeled tree with the local polarities marked.

  The local polarities are generated based on global functions for polarity
  operators. Polarities are +, -, o, and nil. Nil means we don't have polarity
  information which is usually assumed positive."
  (labels
    ((rechelper (curslf pol vars)
       "Heavy lifting helper function.

       Includes an additional argument of the current local polarity context
       determined from the parent."
       (cond
         ;; Atom, just add polarity label.
         ((atom curslf)
          (make-instance 'labeled-tree
                         :expr curslf
                         :label pol))
         ;; Determiner, recurse while ignoring the variable.
         ((det? (first curslf))
          (make-instance
            'labeled-tree
            :expr (mapcar #'(lambda (x y) (rechelper x y (cons (second curslf)
                                                               vars)))
                          curslf
                          ;; Polarities for determiner and variable are nil,
                          ;; the restrictor and body are determined by the
                          ;; determiner.
                          (append '(nil nil)
                                  (determiner-polarity (first curslf))))
            :label pol))
         ;; In variable usage as the first argument.
         ;; Polarity is nil for children (the global polarity will be
         ;; propagated from here).
         ((or (member (first curslf) vars)
              ;; Coordinated phrase.
              ;; Just nil for all.
              (some #'lex-coord? curslf))
          (make-instance
            'labeled-tree
            :expr (mapcar #'(lambda (x y) (rechelper x y vars))
                          curslf
                          (loop for i from 0 below (length curslf) collect nil))
            :label pol))
         ;; Otherwise, recurse with prefix operator.
         (t
          (make-instance
            'labeled-tree
            :expr (mapcar #'(lambda (x y) (rechelper x y vars))
                          curslf
                          (append '(nil)
                                  (operator-polarity (terms-for-vars (first curslf) vars)
                                                     (1- (length curslf)))))
              :label pol)))))
       (rechelper slf '+ nil)))

(defun determiner-polarity (slf)
  ;; NB
  ;; (ABSOLUTE) 'MANY' IS UP-UP,
  ;; RELATIVE 'MANY' IS FLAT-UP,
  ;; (ABSOLUTE) 'FEW' IS DOWN-DOWN,
  ;; RELATIVE 'FEW' IS FLAT-DOWN,
  (cond
    ((member slf '(all.d every.d each.d))
     '(- +))
    ((or (member slf '(a.d an.d some.d many.d one.d two.d three.d several.d a_few.d))
         (ttt:match-expr '(nquan (at_least.mod-a adj?)) slf))
     '(+ +))
    ;; Definites, possessives, absolute numericals.
    ((member slf '(the.d most.d both.d my.d his.d her.d their.d your.d its.d
                         our.d))
     '(o +))
    ((member slf '(neither.d))
     '(o -))
    ((or (member slf '(no.d few.d neither.d))
         (ttt:match-expr '(nquan (at_most.mod-a adj?)) slf))
     '(- -))
    ;; Numerical determiners
    ((and (atom slf) (number-quantifier? slf))
     '(+ +))
    ;; Default to (nil nil) which will generally be assumed to be (+ +).
    (t
     (format t "Unknown polarity for determiner!: ~s~%" slf)
     '(nil nil))))

(defun operator-polarity (slf argnum)
  ;; Generates polarity for non-determiner operators as a list, though this
  ;; will only be a single argument.
  (cond
    ((member slf '(not not.adv-s))
     '(-))
    ;; Operators with a single argument that don't allow polarity propagation:
    ;;  most reifiers, quotes, possessives, etc.
    ((or (member slf '(that |"| |"|) :test #'equal)
         (preposs-macro? slf)
         ;; Generalized for of preposs-macro?. Doesn't check that first
         ;; argument is a term.
         (and (listp slf) (> (length slf) 1) (equal ''s (second slf)))
         (noun-reifier? slf)
         (sent-reifier? slf)
         (tensed-sent-reifier? slf)
         (verb-reifier? slf))
     '(o))
    ;; Verbs, adjectives, nouns, tenses, adverbs, modifiers, sentential
    ;; prepositional phrases default to nil polarity of arguments.
    ((or (lex-tense? slf)
         (verb? slf)
         (tensed-verb? slf)
         (aux? slf)
         (tensed-aux? slf)
         (adj? slf)
         (noun? slf)
         (adv? slf)
         (mod-n? slf)
         (mod-a? slf)
         (lex-rel? slf)
         (term? slf)
         (lex-prep? slf)
         (ps? slf)
         (and (listp slf)
              (lex-ps? (first slf)))
         ;; Badly parsed lex-ps
         (and (listp slf)
              (lex-prep? (first slf))))
     (loop for i from 0 below argnum collect nil))
    ;; Quote for possessive, just nil.
    ((equal 'quote slf) '(nil))
    ;; A bunch of polarity transparent operators.
    ((member slf '(= n+preds np+preds plur pasv perf prog mod-n mod-a adv-a
                     adv-s adv-e adv-f nquan fquan))
     (loop for i from 0 below argnum collect nil))
    ;; Default to (nil) which will generally be assumed to be (+).
    (t
     (format t "Unknown polarity for scoping operator: ~s~%" slf)
     '(nil))))

(defun polarity-diff (mulf1 mulf2)
  "Computes the difference in polarities of the two polarity marked ulfs.
  Counts the number of nodes where the two polarity markings are different.

  Raises an error if the two mulfs have different tree shapes."
  (let ((ulf1 (get-ulf mulf1))
        (ulf2 (get-ulf mulf2)))
    (cond
      ;; Root.
      ((and ulf1 ulf2)
       (if (eql (global-polarity mulf1) (global-polarity mulf2))
         1
         0))
      ;; Recrusive.
      ((and (listp ulf1) (listp ulf2) (= (length ulf1) (length ulf2)))
       (+ (if (eql (global-polarity mulf1) (global-polarity mulf2))
            1
            0)
          (apply #'+ (mapcar #'polarity-diff ulf1 ulf2))))
      ;; Erroneous.
      (t (error "The two inputs of polarity-diff are not the same shape!")))))

(defun filter-badly-placed-dets (sops lulf)
  "Returns a list of sops without badly placed determiners.

  Determiners must be in a (det rest) construction."
  (cond
    ((atom (expr lulf)) sops)
    ;; Some determiner in a bad construction.
    ((and (some #'det? (mapcar #'unlabeled-tree (expr lulf)))
          (not (lulf-det-rest? lulf)))
     ;; Remove all determiner indices from sops.
     (let* ((bad-det-indices
              (mapcar #'label
                      (remove-if-not #'(lambda (child)
                                         (det? (unlabeled-tree child)))
                                     (expr lulf))))
            (new-sops
              (remove-if #'(lambda (sop-idx)
                             (member sop-idx bad-det-indices))
                         sops)))
       ;; Recurse and update new-sops
       (loop for child in (expr lulf)
             do (setf new-sops (filter-badly-placed-dets new-sops child)))
       ;; Return updated sops
       new-sops))
    ;; Basic recursive base.
    (t
     (let ((new-sops sops))
       (loop for child in (expr lulf)
             do (setf new-sops (filter-badly-placed-dets new-sops child)))
       new-sops))))

(defun infer-scoping-from-polarity (mulf)
  "Given a ULF with global polarity markings, infers the scoping of polarity
  operators.

  Returns four values:
  1. The scoping as an association list from scope point indices to scoping
     operator indices
  2. The labeled ULF
  3. The labeled SLF
  4. The polarity marked SLF
  5. The local polarity marking of SLF"
  ;; 1. Get possible scoping for ulf.
  ;; 2. Get marked ulf for each possible scoping.
  ;; 3. Choose the scoping that is closest to the marked ULF
  (let*
    ((ulf (mulf2ulf mulf))
     ;; DFS index the tree for internal node tracking.
     (lulf (dfs-label-tree ulf))
     ;; Get possible scopings.
     (sops (filter-badly-placed-dets
             (get-scoping-op-labels lulf)
             lulf))
     (sconstraints (get-scope-constraints lulf sops))
     (spoints (get-scope-points lulf))
     (pscopings (possible-scopings lulf sconstraints spoints))
     ;; Get marked ULF for each possible scoping.
     (ps-mulf-lslf-mslfs
       (mapcar #'(lambda (ps)
                   (multiple-value-list
                     (polarity-for-scoping lulf ps)))
               pscopings))
     (ps-mulfs (mapcar #'first ps-mulf-lslf-mslfs))
     (ps-lslfs (mapcar #'second ps-mulf-lslf-mslfs))
     (ps-mslfs (mapcar #'third ps-mulf-lslf-mslfs))
     (ps-local-mslfs (mapcar #'fourth ps-mulf-lslf-mslfs))
     min-dist cur-dist cur-mulf min-scoping (min-idx 0))
    ;; Choose the closest scoping.
    (loop for i from 0 below (length ps-mulfs)
          do
          (progn
            (setf cur-dist (polarity-diff mulf (nth i ps-mulfs)))
            (when (or (null min-dist) (< cur-dist min-dist))
              (setf min-dist cur-dist)
              (setf min-scoping (nth i pscopings))
              (setf min-idx i))))
    ;; Return the closest scoping.
    (values min-scoping
            lulf
            (nth min-idx ps-lslfs)
            (nth min-idx ps-mslfs)
            (nth min-idx ps-local-mslfs))))

(defun get-scope-points (lulf)
  "Produces the points in the ulf that can support scoping operators.

  Given a labeled ULF, returns a list of labels for positions where scoping
  operators can be placed.

  Scoping operators can operate over tensed or untensed sentences or below type
  shifters. This ignores any scoping operators that are already in the ULF
  since they will be moved into scoping positions as well.

  Examples:
    [0 ([1 this.pro]
        [2 ([3 ([4 pres] [5 be.v])]
            [6 ([7 =] [8 ([9 a.d] [10 sentence.n])])])])] -> (0), just the root

    [0 ([1 ([2 when.ps]
            [3 ([4 i.pro] [5 ([6 past] [7 see.v])])])]
        [8 ([9 i.pro]
            [10 ([11 past] [12 know.v])])])] -> (0 3 8)

    [0 ([1 surely.adv-s]
        [2 ([3 he.pro] [4 ([5 pres] [6 know.v])])])] -> (2), root without adv-s"
  (labels
    ((rechelper (lulf)
       "Most of the heavy lifting."
       (let ((ulf (unlabeled-tree lulf))
             recreses curres)
         ;; Reached a leaf, just return.
         (when (atom ulf)
           (return-from rechelper nil))
         ;; Recurse and get current result.
         (cond
           ;; Scoping operator on the left side.
           ;; Recurse to the right.
           ((and (= 2 (length ulf))
                 (scoping-op? (first ulf)))
            (setf recreses (list (rechelper (second (expr lulf))))))
           ;; Scoping operator on the right side.
           ;; Recurse to the left.
           ((and (= 2 (length ulf))
                 (scoping-op? (second ulf)))
            (setf recreses (list (rechelper (first (expr lulf))))))
           ;; Type shifter on the left side.
           ;; Recurse to the right and add a scope point.
           ((and (= 2 (length ulf))
                 (type-shifter? (first ulf)))
            (setf curres (list (label (second (expr lulf)))))
            (setf recreses (list (rechelper (second (expr lulf))))))
           ;; No scoping operator or a >2 element list.
           ;; Recurse into all and check if it's a sentence.
           (t
             (when (or (sent? ulf) (tensed-sent? ulf))
               (setf curres (list (label lulf))))
             (setf recreses (mapcar #'rechelper (expr lulf)))))
         ;; Aggregate.
         (apply #'append
                (cons curres recreses)))))
    ;; At the top-level we filter through scoping operators, but then assume a
    ;; sentence before recursing more thoroughly while checking the sentence
    ;; types.
    (let ((ulf (unlabeled-tree lulf))
          recreses curres)
      ;; Reached a leaf, just return.
      (when (atom ulf)
        (return-from get-scope-points nil))
      ;; Recurse and get current result.
      (cond
        ;; Scoping operator on the left side.
        ;; Recurse to the right.
        ((and (= 2 (length ulf))
              (scoping-op? (first ulf)))
         (setf recreses (list (get-scope-points (second (expr lulf))))))
        ;; Scoping operator on the right side.
        ;; Recurse to the left.
        ((and (= 2 (length ulf))
              (scoping-op? (second ulf)))
         (setf recreses (list (get-scope-points (first (expr lulf))))))
        ;; No more scoping operators, assume a sentence, and recurse with more
        ;; strict recursive helper function.
        (t
         (setf curres (list (label lulf)))
         (setf recreses (mapcar #'rechelper (expr lulf)))))
      ;; Aggregate.
      (remove-duplicates
        (apply #'append
               (cons curres recreses))))))


(defun get-scope-constraints (lulf sops)
  "Produces the deepest position of the ULF for each scope operator.

  This uses the node labeling of the ULF for the position as well as the
  operator and outputs an alist. If there are no constraints, then the
  root label is given.

  Examples:
    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
    sops: (2 9)
    -> ((2 . 0)
        (9 . 0))

    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 believe.v])]
                  [8 ([9 that]
                      [10 ([11 ([12 a.d] [13 person.n])]
                           [14 ([15 pres] [16 exist.v])])])])])]
    sops: (2 12)
    -> ((2 . 0)
        (12 . 10))"
  ;; For now, we only restrict by type-shifters.
  (labels
    ((merge-constraints (constraint-list)
       "Merges a list of scope constraint lists into a single constraint list."
       (let ((result (apply #'append constraint-list)))
         ;; The constraints should be unique.
         ;(assert (= (length result)
         ;           (length (remove-duplicates result :key #'car))))
         result))
     (type-shifter-constraints (curlulf deepest-preshift)
       "Computes type shifter constraints for the scope operators (sops).

       Tracks deepest pre-shift position for any constituent of the ULF."
       (let (curres recres recres1 recres2)
         ;; If we are at a scope operator, get constraint.
         (when (member (label curlulf) sops)
           (setf curres (list (cons (label curlulf) deepest-preshift))))
         ;; Recurse as relevant.
         (when (listp (expr curlulf))
           (cond
             ;; Current ulf starts with a type-shifter.
             ;; Recurse into all but first with a constraint change and
             ;; into the first without a constraint change.
             ((type-shifter? (unlabeled-tree (first (expr curlulf))))
              (setf recres1 (mapcar (lambda (lu) (type-shifter-constraints
                                                   lu (label lu)))
                                    (cdr (expr curlulf))))
              (setf recres2 (type-shifter-constraints (first (expr curlulf))
                                                      deepest-preshift))
              ;; Merge results into a single result.
              (setf recres (merge-constraints (cons recres2 recres1))))
             ;; Otherwise, just recurse without constraint change.
             (t
              (setf recres1 (mapcar (lambda (lu) (type-shifter-constraints
                                                   lu deepest-preshift))
                                    (expr curlulf)))
              (setf recres (merge-constraints recres1)))))
         (merge-constraints (list curres recres)))))

     ;; Main body of get-scope-constraints
     (type-shifter-constraints lulf 0)))


(defun possible-scopings (lulf scope-constraints scope-points)
  "Produces possible scopings with an internal indexed ULF representation.

  Inputs:
    lulf: An index labeled ULF formula
    scope-constraints: A list of DFS indices of ulf that corresponds to scoping
      operators and its constraint. A constraint is a node index for each
      scoping operator that cannot be scoped over. 0 for no constraints.
    scope-points: A list of indices that corresponds to possible scoping
      points. The scope operators would scope directly over this node. These
      should be sentences/tensed sentences including all possible modifiers.
      The values are monotonically decreasing, which allows efficient recursive
      construction.

  Outputs:
    A list of possible scopings. Each possible scoping is an alist from scope
    point to an ordered list of indices of scoping operators.

  Example:
    lulf: [0 ([1 ([2 every.d] [3 man.n])]
              [4 ([5 ([6 pres] [7 own.v])] [8 ([9 a.d] [10 shirt.n])])])]
    scope-constraints: ((2 . 0)
                        (9 . 0))
    scope-points: (0)

    output: (((0 . (2 9)))
             ((0 . (9 2))))"
  ;; - DFS through and keep track of scope points from root to current position.
  ;; - Strip off positions prior to the scope constraint.
  ;; - Recurse if needed
  ;; - Use the remaining scope points and recursed result to create a new list of
  ;;   possible scopings with the current index included.
  (labels
    ((filter-non-recursive-indices (res curidx)
       "Filters non-recursive scope operator indices from a possible scoping result.
       This is used to prepare the result for merging with other recursive results."
       (mapcar (lambda (alst)
                 (mapcar (lambda (cell)
                           (cons (car cell)
                                 (remove-if (lambda (x) (<= x curidx))
                                            (cdr cell))))
                         alst))
               res))
     (possible-list-interleavings (lst1 lst2)
       "Returns a list of possible interleavings of two ordered lists."
       (cond
         ;; Base cases.
         ((and (null lst1) (null lst2)) (list nil))
         ((null lst1) (list lst2))
         ((null lst2) (list lst1))
         ;; Recursive case.
         (t (append
              (let ((lrecres (possible-list-interleavings (cdr lst1) lst2)))
                (mapcar (lambda (x) (cons (car lst1) x)) lrecres))
              (let ((rrecres (possible-list-interleavings lst1 (cdr lst2))))
                (mapcar (lambda (x) (cons (car lst2) x)) rrecres))))))
     (merge-scope-asgn-rechelper (asgn1 asgn2 remkeys acc)
       "Recursive helper function for merge-scoping-assignments.
       Keeps track of the remaining keys and the accumulated results.

       Returns the possibilities for each key, but not the resulting alists.
       For each key, k, in both alists,
        if k is only in asgn1 or asgn2
          add k to result
        if k is in both
          generate all possible interleavings of keys."
       ;; Base case.
       (when (null remkeys)
         (return-from merge-scope-asgn-rechelper acc))
       ;; Recursive case.
       ;; For each merge merge values as necessary.
       (let* ((k (car remkeys))
              (curcons
                (cond
                  ;; Complex merge case.
                  ((and (assoc k asgn1) (assoc k asgn2))
                   (cons k (possible-list-interleavings (cdr (assoc k asgn1))
                                                        (cdr (assoc k asgn2)))))
                  ;; Simple cases, just take the one that exists as the only
                  ;; possibility.
                  ((assoc k asgn1) (cons k (list (cdr (assoc k asgn1)))))
                  ((assoc k asgn2) (cons k (list (cdr (assoc k asgn2)))))
                  (t (error "Unknown scope assignment for merging!")))))
         ;; Recurse.
         (merge-scope-asgn-rechelper
           asgn1 asgn2 (cdr remkeys) (cons curcons acc))))
     (expand-alist-possibilities (poss-alist)
       "Expands an alist of possible values to a list of alists.
       An alist from key to list of values to a list of alists of key to
       value."
       ;; Base case.
       (when (null poss-alist)
         (return-from expand-alist-possibilities '(nil)))
       ;; Recursive.
       (let ((recres (expand-alist-possibilities (cdr poss-alist)))
             (cur (car poss-alist)))
         (apply #'append
                (mapcar #'(lambda (currec)
                            (mapcar #'(lambda (cur-poss) (cons (cons (car cur) cur-poss)
                                                               currec))
                                    (cdr cur)))
                        recres))))
     (merge-scoping-assignments (asgn1 asgn2)
       "Merges two scoping assignments returns a list of possible merges.
       This is a subroutine of merging possible scoping results, which are
       lists of scoping assignments. Assumes that assignments have unique
       scoping operators."
       (let* ((keys (union (alist-keys asgn1) (alist-keys asgn2)))
              (key-possibilities (merge-scope-asgn-rechelper
                                   asgn1 asgn2 keys nil)))
         ;; Generate the actual alists from the possibilities.
         (expand-alist-possibilities key-possibilities)))
     (merge-results (reslst curidx)
       "Merge a list of recursive results into a single output.
       Each result is a list of alists, representing a list of scope
       assignments. Each scope assignment is an alist from scope point to an
       ordered list of scope operator indices.

       To merge, each alist in the ith index of reslst is combined with each
       alist in every other index of reslst, possibly generating multiple for
       different interleavings, e.g.
        ((0 . (2 9))) and ((0 . (3)))
         -> (((0 . (3 2 9))) (0 . (2 3 9)) (0 . (2 9 3)))

       Both will share instances of scope operators that are less than or equal
       to the current index, so filter those out on all but one to avoid
       duplicates.

       Implemented by recursing lists and only in the base case do we keep the
       indices."
       ;; Base case 1.
       ;; We're left with nothing, return a list of nil, i.e. a single
       ;; assignment with no entries.
       (when (or (equal nil reslst) (equal (list nil) reslst))
         (return-from merge-results (list nil)))
       ;; Base case 2.
       ;; We're left with a list of a single result, i.e. a list with a single
       ;; list of alists. Just return that result.
       (when (= 1 (length reslst))
         (return-from merge-results (car reslst)))
       ;; Recursive case.
       (let (;; Recursive result.
             (recres (merge-results (cdr reslst) curidx))
             ;; Current item with the non-recursive operators filtered.
             (curres (filter-non-recursive-indices (car reslst) curidx)))
         (when (null curres)
           (setf curres (list nil)))
         ;; Merge with recursive results.
         (apply #'append
                (mapcar (lambda (rec-inst)
                          (apply #'append
                                 (mapcar (lambda (cur-inst)
                                           (merge-scoping-assignments rec-inst cur-inst))
                                         curres)))
                        recres))))
     (position-in-ordered-list (val lst)
       "Returns the position of the value in the ordered list, giving the
       position of the nearest larger value if not in the list."
       (loop for e in lst
             for i from 0
             do (when (<= val e)
                  (return-from position-in-ordered-list i))))
     (rechelper (curulf curpoints)
       "Recursive helper function which does most of the heavy lifting.

       curulf: The current labeled ulf in the traversal.
       curpoints: The scope points available from here to the root of the ulf."
       (let (newpoints recres)
         ;; If relevant recurse into the children.
         (when (not (atom (expr curulf)))
           (setf newpoints
                 (if (member (label curulf) scope-points)
                   (sort
                     (remove-duplicates
                       (cons (label curulf) curpoints))
                     #'<)
                   curpoints))
           (setf recreses
                 (mapcar #'(lambda (child) (rechelper child newpoints))
                         (expr curulf)))
           (setf recres (merge-results recreses (label curulf)))
           recres)
         ;; If the current position is a scoping operator (in
         ;; scope-constraints) get all scope positions given any scope
         ;; constraint and interleave with recursive result.
         (if (not (null (assoc (label curulf) scope-constraints)))
           (let*
             ((constraint-idx (cdr (assoc (label curulf) scope-constraints)))
              (scope-positions
                 (subseq curpoints
                         (position-in-ordered-list
                           constraint-idx curpoints)))
              ;; Generate a result format with current position only.
              ;; A list of alists with a single entry each.
              (curres (mapcar #'(lambda (pos) (list (cons pos (list (label curulf)))))
                              scope-positions))
              result)
             ;; Merge current result with recursive results as if they were
             ;; separate recursive results.
             (setf result (merge-results (list recres curres) (1- (label curulf))))
             result)
           ;; If not a scoping operator, just return the recursive result.
           recres))))
    ;; Start recursive process with top-most scope point available.
    ;; This assumes that the ULF is a sentence and allows handling of
    ;; scoping operators that occur above the top-most scope point.
    (rechelper lulf (list (first scope-points)))))

;; A hash table storing previously computed ULF polarity markings.
(defparameter *ulf-marking-result-ht* (make-hash-table :test #'equal))
;; A global hash table of dispatch functions.
;; The dispatch functions are registered by each use case.
(defparameter *ulf-marking-dispatch-fn-ht* (make-hash-table :test #'equal))

(defun get-marked-ulf (ulf &optional dispatch-key)
  "Retrieves a polarity marking for a ULF through increasingly expensive methods.

  1. Checks a hash table of previously computed results.
  2. If the user provides a function based on their specific use case, use that.
  3. Otherwise, use the generic marking through ULF2English and Stanford CoreNLP."
  (let (result)
    (cond
      ;; 1. A marking has been saved.
      ((setf result (gethash ulf *ulf-marking-result-ht*))
       result)
      ;; 2. The ulf is a premise.
      ;; Use generic marking.
      ((member ulf *ulf-premises* :test #'equal)
       (setf result (mark-ulf ulf))
       (setf (gethash ulf *ulf-marking-result-ht*) result) ; save result
       result)
      ;; 2. Checks for dispatch function if a key is given.
      ((and dispatch-key
            (gethash dispatch-key *ulf-marking-dispatch-fn-ht*)
            (setf result (funcall (gethash dispatch-key
                                           *ulf-marking-dispatch-fn-ht*)
                                  ulf)))
       (setf (gethash ulf *ulf-marking-result-ht*) result) ; save result
       result)
      ;; 3. Generic marking.
      (t
       (setf result (mark-ulf ulf))
       (setf (gethash ulf *ulf-marking-result-ht*) result) ; save result
       result))))

;; Hash table from ULF to the results from infer-scoping-from-polarity + the marked ulf.
;; 1. The polarity marked ulf
;; 2. The scoping
;; 3. The labeled ULF
;; 4. the labeled SLF
;; 5. The polarity marked slf
(defparameter *ulf-scoping-info-ht* (make-hash-table :test #'equal))
(defun initial-inferred-polarity-processing (ulfs goal)
  "Initial processing of a list of ULFs for inferred scoping/polarity.

  This computes the scoping and polarity determination for the initial ULFs
  based on the Natlog polarity function which are used to the infer and
  results."
  (declare (ignore goal))
  ;; 1a. Run natlog as a batch to reduce JVM overhead and initialize memo.
  (when (eql *global-natlog-memo-flag* :off)
    (warn (concatenate
            'string
            "The global memoization flag (*global-natlog-memo-flag*) is set "
            "to :off. Please set of :on or :local for the batched natlog "
            "processing to have its intended effect.")))
  (run-natlog-ulfs ulfs :memoize t)
  (let*
    (;; 1b. Get marked ULFs.
     ;; This will used the memoized results of the batched natlog processing.
     (mulfs (mapcar #'mark-ulf ulfs))
     )
    ;; 1c. Add ulfs to the ulf-premises.
    (setf *ulf-premises* ulfs)
    (values ulfs goal)))

