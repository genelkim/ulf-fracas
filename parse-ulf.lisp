;;; Gene Kim, 2020-10-16
;;;
;;; Main ULF parsing functions for ulf-fracas. Uses the lenulf package to get the
;;; initial parses and cleans it up to only include what is needed for the
;;; ulf-fracas package and performs some post-processing.

(in-package :ulf-fracas)

(defun remove-token-index (idxsym &key (delim #\~))
  "Removes the indexing from a symbol token, assuming the indexing is provided
  with a delimiter at the end of the symbol. Default delimiter, tilde."
  (let ((symstr (symbol-name idxsym)))
    (intern (subseq symstr 0 (position delim symstr :from-end t))
            :ulf-fracas)))

(defun remove-token-indices (idxulf &key (delim #\~))
  "Removes the indexings from a ulf, assuming the indexing is provided with a
  delimiter at the end of the symbol. Default delimiter, tilde."
  (cond
    ((null idxulf) idxulf)
    ((atom idxulf) (remove-token-index idxulf :delim delim))
    (t (mapcar #'(lambda (x) (remove-token-indices x :delim delim))
               idxulf))))

(defun canonicalize (ulf)
  "Canonicalizes ULF variations for the ulf-fracas package.

  Operations:
  1. Lift negations
  "
  (lift-not ulf))

(defparameter *debug-parse-ulf* nil)

(defun parse-ulf (str &key (synparser "k&k"))
  "Parse an English sentence into ULF.

  Uses the lenulf package for the core annotation, which is then post-processed."
  (let*
    ((rawparse
       ;; The lenulf package requires *package* to be :lenulf
       (let ((*package* (find-package :lenulf)))
         (with-python-env ('benepar)
           (intern-symbols-recursive (if synparser
                                       (lenulf:english-to-ulf str :synparser synparser)
                                       (lenulf:english-to-ulf str))
                                     :ulf-fracas))))
     (token-removed (remove-token-indices rawparse))
     (fixed
       (with-python-env ('ulf2english)
         (standardize-ulf token-removed :pkg :ulf-fracas)))
     (canon (canonicalize fixed)))
    (when *debug-parse-ulf*
      (format t "======parse-ulf stages======~%")
      (format t "raw: ~s~%token-removed: ~s~%fixed: ~s~%canon: ~s~%"
              rawparse token-removed fixed canon))
    canon))

;;
;; PROCESSING RULES THAT SHOULD BE MOVED TO ULF-LIB ONCE COMPLETE
;;

(defun direct-not-stop? (x)
  "Predicate for operators that stop 'not' directly below it."
  (or (advformer? x)
      (detformer? x)
      (modformer? x)
      (sent-reifier? x)
      (noun-reifier? x)
      (verb-reifier? x)
      (tensed-sent-reifier? x)))

(defun arg-not-stop? (x)
  "Predicate for operators that stop 'not' at its arguments."
  (member x '(n+preds np+preds sub rep qt-attr)))

(defun not-stop? (x) (or (direct-not-stop? x) (arg-not-stop? x)))

(defparameter *ulf-nots* '(not not.adv-s))
(defun ulf-not? (x) (member x *ulf-nots*))

(defun lift-not (ulf)
  "Lift each \"not\" in the given ULF to the appropriate, non-floating level.
  The negation may still not be operating over a sentence if it is locally
  modifying a predicate or verb phrase. This is either explicitly marked with
  local bracketing or may occur because a type-shifter or macro stops further
  lifting.

  Examples:
    (not (every.d person.n) (pres know.v))
    -> (not ((every.d person.n) (pres know.v)))
    (he.pro ((pres know.v) (that (i.pro ((pres be.v) not green.a)))))
    -> (he.pro ((pres know.v) (that (not (i.pro ((pres be.v) green.a))))))
    (i.pro ((pres know.v) (to (not sleep.v))))
    -> (i.pro ((pres know.v) (to (not sleep.v))))
    (the.d (n+preds man.n (sub tht.rel (i.pro ((pres do.aux-s) not (know.v *h))))))
    -> (the.d (n+preds man.n (sub tht.rel (not (i.pro ((pres do.aux-s) (know.v *h)))))))

  High-level recursive algorithm description:
    Call with a ULF, returns a pair
     1. The ULF with all negations lifted or removed if lifted position is not found.
     2. The negations that need placing.

    Base case:
     - atom, return atom and nil

    Recursive case:
     Call recursively on all list elements
     - if there are negations to be placed and current list starts with a lift-stopping operator, place negations to be lifted appropriately,
     There are two types of lift-stopping operator, one which placed negations
     right below it (e.g. type-shifters) and those that place them right
     around one of its arguments (e.g. n+preds).
     - list of length >2 with a negation in it, extract negations and add to lists before returning

    At the top-level just add on all the negations.
  "
  (labels
    ((add-negs (expr negs)
       (reduce #'list negs :initial-value expr :from-end t))

     (rechelper (form)
       (cond
         ;; Base case
         ((atom form)
          (list form nil))
         ;; Simple recursive case (no lift-stopping and length <= 2 or no negations)
         ;; Recurse and return the removed ULFs together and append negation lists.
         ((and (not (not-stop? (first form)))
               (or (<= (length form) 2)
                   (not (find-if #'ulf-not? form))))
          (let ((recres (mapcar #'rechelper form)))
            (list (mapcar #'first recres)
                  (apply #'append (mapcar #'second recres)))))
         ;; Simple-ish recursive case (no lift-stopping but has negations)
         ;; Recurse and remove current negations.
         ((not (not-stop? (first form)))
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (rechelper nonot))
                 (reculf (first recres))
                 (recnots (second recres)))
            (list reculf (append nots recnots))))
         ;; Direct lift-stopping
         ((direct-not-stop? (first form))
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (mapcar #'rechelper nonot))
                 (newulf (mapcar #'first recres))
                 (allnots (append nots (apply #'append (mapcar #'second recres)))))
            (assert (= (length newulf) 2) (newulf)
                    "All direct not stopping operators must have a single argument: ~s~%" newulf)
            (list
              ;; ULF with negations added below operator.
              (list (car newulf)
                    (add-negs (second newulf) allnots))
              nil)))
         ;; Argument lift-stopping.
         ;; NB: negations not in the arguments are lifted.
         (t
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (mapcar #'rechelper nonot))
                 (negadded-args
                   (mapcar
                     #'(lambda (argres) (add-negs (first argres) (second argres)))
                     (cdr recres))))
            (list
              ;; ULF with negation in args
              (cons (first form) negadded-args)
              nots))))) ; end of rechelper
     ) ; end of labels definitions

    ; labels body
    ; Recurse and add in any stray negations.
    (let* ((recres (rechelper ulf))
           (newulf (first recres))
           (nots (second recres)))
      (add-negs newulf nots))))

