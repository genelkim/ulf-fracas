;;; Gene Kim 2020-09-10
;;; The aristotelian syllogisms from the Appnedix of Monotonic Inference for
;;; Underspecified Episodic Logic. Gene Louis Kim, Mandar Juvekar, and Lenhart
;;; Schubert.
;;;
;;; Syllogism 1 (Barbara). Every M is a P and Every S is a M entails Every S is a P.
;;; Syllogism 2 (Darii). Every M is a P and Some S is a M entails Some S is a P.
;;; Syllogism 3 (Celarent). No M is a P and Every S is a M entails No S is a P.
;;; Syllogism 4 (Ferio). No M is a P and Some S is a M entails Not every S is a P.

(in-package :ulf-fracas)

(defclass syllogism ()
  ((name
     :initarg :name
     :accessor name)
   (premises
     :initarg :premises
     :accessor premises)
   (conclusion
     :initarg :conclusion
     :accessor conclusion)))

(defparameter *ulf-aristotelian-syllogisms*
  (list
    ;; Syllogism 1 (Barbara).
    ;; Every M is a P and Every S is a M entails Every S is a P.
    (make-instance 'syllogism
                   :name "Barbara"
                   :premises '(((every.d m.n) ((pres be.v) (= (a.d p.n))))
                               ((every.d s.n) ((pres be.v) (= (a.d m.n)))))
                   :conclusion '((every.d s.n) ((pres be.v) (= (a.d p.n)))))
    ;; Syllogism 2 (Darii).
    ;; Every M is a P and Some S is a M entails Some S is a P.
    (make-instance 'syllogism
                   :name "Darii"
                   :premises '(((every.d m.n) ((pres be.v) (= (a.d p.n))))
                               ((some.d s.n) ((pres be.v) (= (a.d m.n)))))
                   :conclusion '((some.d s.n) ((pres be.v) (= (a.d p.n)))))
    ;; Syllogism 3 (Celarent).
    ;; No M is a P and Every S is a M entails No S is a P.
    (make-instance 'syllogism
                   :name "Celarent"
                   :premises '(((no.d m.n) ((pres be.v) (= (a.d p.n))))
                               ((every.d s.n) ((pres be.v) (= (a.d m.n)))))
                   :conclusion '((no.d s.n) ((pres be.v) (= (a.d p.n)))))
    ;; Syllogism 4 (Ferio).
    ;; No M is a P and Some S is a M entails Not every S is a P.
    (make-instance 'syllogism
                   :name "Ferio"
                   :premises '(((no.d m.n) ((pres be.v) (= (a.d p.n))))
                               ((some.d s.n) ((pres be.v) (= (a.d m.n)))))
                   :conclusion '(not ((every.d s.n) ((pres be.v) (= (a.d p.n))))))))

;; NB: Some of the names in the formulas were changed because they interacted
;; badly with some of the ULF fixing measures in the lenulf parser.
(defparameter *string-aristotelian-syllogisms*
  (list
    ;; Syllogism 1 (Barbara).
    ;; Every M is a P and Every S is a M entails Every S is a P.
    (make-instance 'syllogism
                   :name "Barbara"
                   :premises '("Every M is a P"
                               "Every S is a M")
                   :conclusion "Every S is a P")
    ;; Syllogism 2 (Darii).
    ;; Every M is a P and Some S is a M entails Some S is a P.
    (make-instance 'syllogism
                   :name "Darii"
                   :premises '("Every M is a P"
                               "Some N is a M")
                   :conclusion "Some N is a P")
    ;; Syllogism 3 (Celarent).
    ;; No M is a P and Every S is a M entails No S is a P.
    (make-instance 'syllogism
                   :name "Celarent"
                   :premises '("No M is a P"
                               "Every N is a M")
                   :conclusion "No N is a P")
    ;; Syllogism 4 (Ferio).
    ;; No M is a P and Some S is a M entails Not every S is a P.
    (make-instance 'syllogism
                   :name "Ferio"
                   :premises '("No M is a P"
                               "Some N is a M")
                   :conclusion "Not every N is a P")))

;; Generates the combinations premises and conclusions by moving negations of
;; premises to conclusions and vise-versa through
;;    ((X ^ !A) -> Y) <-> (X -> (A v Y))
;; This corresponds to the negation rule used by Sanchez-Valencia (1991). For
;; this function, we assume that there is one conclusion and multiple premises,
;; and all combinations will follow suit.
(defun premise-conclusion-combinations (sylm)
  (let ((all-antecedents (cons (list 'not (conclusion sylm))
                               (premises sylm)))
        (combinations nil))
    (loop for i from 0 to (1- (length all-antecedents)) do
          (let ((new-conclusion (list 'not (nth i all-antecedents)))
                (new-premises (append (subseq all-antecedents 0 i)
                                      (subseq all-antecedents (1+ i)))))
            (setf combinations (cons (list new-premises new-conclusion)
                                     combinations))))
    combinations))

(defun run-ulf-aristotelian-syllogism (&key (steps 3) (verbosity 1))
  "Run the fully-symbolic aristotelian syllogism inferences with the nlog
  inference system. Includes premise-conclusion shuffling through negations to
  handle the Ferio inference. Tries every combination of two premises and a
  single conclusion where we can move a premise to a conclusion and vise-versa
  through negation. There is a subtlty of conjunctions and disjunctions in this
  process, which we bypass by only allowing a single formula in the hypothesis.

  Arguments
    steps: Maximum number of forward inference steps.
    verbosity:
      0 - silent except for success/failure
      1 - high-level messages
      2 - detailed messages
  "
  (let ((*unary-rules* (list #'uci-list
                             #'neg-elim
                             #'neg-intro
                             #'no2notindef))
        (*binary-rules* (list #'umi-positive-polarity
                              #'umi-negative-polarity))
        (correct 0))
    (princln "Running symbolic Aristotelian syllogism inferences...")
    (loop for sylm in *ulf-aristotelian-syllogisms* do
          (let ((pc-combs (premise-conclusion-combinations sylm))
                (pc-idx 0)
                (success nil))
            (when (> verbosity 0)
              (princln "============================")
              (format t "Syllogism ~a~%" (name sylm)))
            (loop for pc in pc-combs do
                  ; High-level messages
                  (when (> verbosity 0)
                    (princln "")
                    (format t "premise-conclusion combination: ~s~%" pc-idx)
                    (princln "Premises:")
                    (loop for premise in (first pc)
                          do (format t "  ~s~%" premise))
                    (format t "Expected conclusion:~%  ~s~%" (second pc)))

                  (multiple-value-bind (res infs step-no srcht) (goal-directed-infer-nlog (first pc)
                                                                                          (second pc)
                                                                                          :max-steps steps
                                                                                          :callpkg :ulf-fracas/tests
                                                                                          :track-sources t)
                    ; Print inferences and source hash table for verbose output.
                    (when (> verbosity 1)
                      ;(princln "Inferences:")
                      ;(loop for inf in infs
                      ;      do (format t "  ~s~%" inf))
                      ;(princln "")

                      (princln "Inference source hash table:")
                      (loop for key being the hash-keys of srcht
                            using (hash-value value)
                            do (format t "  ~S:~%    ~S~%" key value)))

                    (if res
                      (progn
                        (format t "Combination ~s of syllogism ~a succeeded in ~s steps!~%" pc-idx (name sylm) step-no)
                        (setf success t)
                        (loop-finish))
                      (format t "Combination ~s of syllogism ~a failed!~%" pc-idx (name sylm)))

                    (incf pc-idx)))

            (when success
              (incf correct))))

    (princln "")
    (format t "~s out of ~s syllogisms succeeded~%"
            correct (length *ulf-aristotelian-syllogisms*))
    (format t "Accuracy: ~a~%"
            (float (/ correct (length *ulf-aristotelian-syllogisms*))))
    ;; Return the number of correct inferences.
    (values correct (/ correct (length *ulf-aristotelian-syllogisms*)))))

(defun interpret-syllogism-data (sylm &optional (verbosity 0))
  "Interprets English components of the syllogism data with the ULF parser and
  returns a new syllogism task with the interpreted data."
  (when (> verbosity 0)
    (princln "Interpreting syllogism data..."))
  (make-instance 'syllogism
                 :name (name sylm)
                 :premises (mapcar #'parse-ulf (premises sylm))
                 :conclusion (parse-ulf (conclusion sylm))))

(defun run-string-aristotelian-syllogism (&key (steps 3) (verbosity 1))
  "Run the fully-symbolic aristotelian syllogism inferences with the nlog
  inference system. Includes premise-conclusion shuffling through negations to
  handle the Ferio inference. Tries every combination of two premises and a
  single conclusion where we can move a premise to a conclusion and vise-versa
  through negation. There is a subtlty of conjunctions and disjunctions in this
  process, which we bypass by only allowing a single formula in the hypothesis.

  Arguments
    steps: Maximum number of forward inference steps.
    verbosity:
      0 - silent except for success/failure
      1 - high-level messages
      2 - detailed messages
  "
  (let ((*unary-rules* (list #'uci-list
                             #'neg-elim
                             #'neg-intro
                             #'no2notindef))
        (*binary-rules* (list #'umi-positive-polarity
                              #'umi-negative-polarity))
        (correct 0))
    (princln "Running symbolic Aristotelian syllogism inferences...")
    (loop for sylm in *string-aristotelian-syllogisms* do
          (let* ((newsylm (interpret-syllogism-data sylm verbosity))
                 (pc-combs (premise-conclusion-combinations newsylm))
                 (pc-idx 0)
                 (success nil))
            (when (> verbosity 0)
              (princln "============================")
              (format t "Syllogism ~a~%" (name sylm))
              (format t "  Raw Premises: ~s~%" (premises sylm))
              (format t "  Raw Conclusion: ~s~%" (conclusion sylm)))
            (loop for pc in pc-combs do
                  ; High-level messages
                  (when (> verbosity 0)
                    (princln "")
                    (format t "premise-conclusion combination: ~s~%" pc-idx)
                    (princln "Premises:")
                    (loop for premise in (first pc)
                          do (format t "  ~s~%" premise))
                    (format t "Expected conclusion:~%  ~s~%" (second pc)))

                  (multiple-value-bind (res infs step-no srcht) (goal-directed-infer-nlog (first pc)
                                                                                          (second pc)
                                                                                          :max-steps steps
                                                                                          :callpkg :ulf-fracas/tests
                                                                                          :track-sources t)
                    ; Print inferences and source hash table for verbose output.
                    (when (> verbosity 1)
                      ;(princln "Inferences:")
                      ;(loop for inf in infs
                      ;      do (format t "  ~s~%" inf))
                      ;(princln "")

                      (princln "Inference source hash table:")
                      (loop for key being the hash-keys of srcht
                            using (hash-value value)
                            do (format t "  ~S:~%    ~S~%" key value)))

                    (if res
                      (progn
                        (format t "Combination ~s of syllogism ~a succeeded in ~s steps!~%" pc-idx (name sylm) step-no)
                        (setf success t)
                        (loop-finish))
                      (format t "Combination ~s of syllogism ~a failed!~%" pc-idx (name sylm)))

                    (incf pc-idx)))

            (when success
              (incf correct))))

    (princln "")
    (format t "~s out of ~s syllogisms succeeded~%"
            correct (length *ulf-aristotelian-syllogisms*))
    (format t "Accuracy: ~a~%"
            (float (/ correct (length *ulf-aristotelian-syllogisms*))))
    ;; Return the number of correct inferences.
    (values correct (/ correct (length *ulf-aristotelian-syllogisms*)))))

