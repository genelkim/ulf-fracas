(in-package :ulf-fracas)

;;
;; JSON data accessors
;;

(defun get-premises (jentry)
  "Gets the premise texts from a json evaluation entry."
  (let ((premise-keys '(p-1 p-2 p-3 p-4 p-5 p-6)))
    (remove-if #'null
               (mapcar #'(lambda (pkey)
                           (if (assoc pkey jentry)
                             (cdrassoc 'text (cdrassoc pkey jentry))))
                       premise-keys))))
(defun get-hypothesis (jentry)
  "Gets the hypothesis text from a json evaluation entry."
  (cdrassoc 'text (cdrassoc 'h jentry)))

;;
;; Batch preprocessing
;;

(defun batch-natlog-inputs (jdat)
  "Runs natlog polarity marking for premises and hypothesis on the given JSON
  data. The results are memoized so that during the experiment these values
  can simply be looked up.

  The return value is undefined."
  ;; Make sure the major functions are memoized.
  (format t "Running natlog in batch~%")
  (memoize 'run-natlog)
  (memoize 'parse-ulf)
  (setf cl-progress-bar:*progress-bar-enabled* t)
  (let* ((all-sents
           (remove-duplicates
             (apply #'append
                    (cons
                      (mapcar #'get-hypothesis jdat)
                      (mapcar #'get-premises jdat)))
             :test #'equal))
         (all-ulfs
           (cl-progress-bar:with-progress-bar
             ((length all-sents) "Parsing ~s sentences...~%" (length all-sents))
             (mapcar #'(lambda (x) (let ((res (parse-ulf x)))
                                     (cl-progress-bar:update 1)
                                     res))
                     all-sents))))
    (format t "Computing natlog polarities...~%")
    (run-natlog-ulfs all-ulfs)
    (format t "Done!~%")))

;;
;; Functions to evaluate against gold labels and print SUCCESS/FAILURE for each
;; and overall precision.
;;

(defun eval-gold (&key (filepath "data/fracas.json") (sections '("GQ"))
                                  (steps 3) (search-method :heuristic)
                                  (ids nil))
  "Evaluates inferences results against the gold labels."
  (let* ((json:*json-symbols-package* (find-package :ulf-fracas))
         (jdat (with-open-file (s filepath) (json:decode-json s)))
         (res-set nil)
         (all-results nil) ; alist of id to alist of values: expected, actual, steps
         (filtered-data (filter-data jdat 'group sections))
         (filtered-data (filter-data filtered-data 'answer '("yes" "no" "unknown")))
         (filtered-data (filter-data filtered-data 'id ids))
         (timestamp-str
					 (multiple-value-bind
						 (second minute hour date month year day-of-week dst-p tz)
						 (get-decoded-time)
						 (format nil "~s-~s-~s-~s:~s:~s"
										 month date year hour minute second)))
         ans success? msg res
         p-ulf h-ulf p-key
         cur-step-no steps-list)

    ;; Run natlog in batch.
    (batch-natlog-inputs filtered-data)

    (loop for case in filtered-data do
      (progn
        (handler-case
          (progn
            (format t "~%==Info for ID: ~s==~%" (cdrassoc 'id case))
            (setq p-ulf nil)
            (loop for i in '(1 2 3 4 5 6) do
              (setq p-key (read-from-string (concatenate 'string "p-" (write-to-string i))))
              (when (assoc p-key case)
                (setq p-ulf (cons (parse-ulf (cdrassoc 'text (cdrassoc p-key case)))
                                  p-ulf))
                (format t "[~a] Premise: ~s~%" i (cdrassoc 'text (cdrassoc p-key case)))
                (format t "[~a] ULF: ~s~%" i (car p-ulf))))
            (setq h-ulf (parse-ulf (cdrassoc 'text (cdrassoc 'h case))))
            (format t "Hypothesis: ~s~%" (cdrassoc 'text (cdrassoc 'h case)))
            (format t "ULF: ~s~%" h-ulf)
            (multiple-value-bind
              (rawres infs step-no srcht)
              (case search-method
                (:simple-goal-directed (goal-directed-infer-nlog p-ulf
                                                                 h-ulf
                                                                 :max-steps steps
                                                                 :track-sources t))
                (:heuristic (common-api-heuristic-search p-ulf
                                                         h-ulf
                                                         :max-iterations steps
                                                         :heuristic #'unordered-leaf-f1-new-only-heuristic
                                                         :track-sources t)))
              (setq res (get-answer rawres))
              (setq cur-step-no step-no)
              (push step-no steps-list)))
          (error (e)
            (if res
              (format t "Result: ~s~%" res)
              (setq res nil))
            (format t "Error: ~s~%" e)
            (error e)))

        (setq ans (cdrassoc 'answer case))
        (setf success? (string-equal ans res))
        (setf msg (concatenate 'string
                                (format nil
                                        (concatenate 'string
                                                     (if success? "SUCCESS" "FAILURE")
                                                     " in ~s steps~%")
                                        cur-step-no)
                                (format nil "Result: ~a~%" res)
                                (format nil "Answer: ~a~%" ans)))
        (if success?
          (syspas (format t msg))
          (syserr (format t msg)))
        (setf res-set (append res-set (list success?)))
        (setf all-results
              (acons (cdrassoc 'id case)
                     (list (cons 'success success?)
                           (cons 'expected ans)
                           (cons 'predicted res)
                           (cons 'steps cur-step-no))
                     all-results))))

    (format t "~%======SUMMARY=======~%")
    (sysmsg (format t "Precision: ~s~%" (cl-util:precision res-set)))
    (sysmsg (format t "Average number of steps~%"))
    (sysmsg (format t "    w/  unknowns: ~s~%"
                    (float (/ (reduce #'+ steps-list) (length steps-list)))))
    (sysmsg (format t "    w/o unknowns: ~s~%"
                    (let ((no-unks (remove-if #'(lambda (x) (>= x steps))
                                              steps-list)))
                      (float (/ (reduce #'+ no-unks) (length no-unks))))))

    ;; Write all-results to file.
    (with-open-file (s (concatenate 'string "results/results-" timestamp-str ".json")
                       :if-does-not-exist :create
                       :direction :output)
      (format s (json:encode-json-to-string all-results)))
    ))

(defun eval-gold-fracas (&key (filepath "data/fracas.json") (sections '("GQ"))
                              (steps 50) (ids '()))
  (format t "Running FraCaS experiment for ~s...~%" sections)
  (eval-examples :verbose nil
                 :filepath filepath
                 :sections sections
                 :steps steps
                 :ids ids))

;;
;; Gene's more verbose version for parser development.
;;

;; Returns t if example succeeds, nil otherwise.
(defun run-verbose-example (entry &key (steps 3) (search-method :heuristic))
  (let* ((premises (get-premises entry))
         (hypothesis (get-hypothesis entry))
         (id (cdrassoc 'id entry))
         premise-ulfs hypothesis-ulf)
    (princln "====================================")
    (format t "~s example, index: ~s~%" (cdrassoc 'group entry) id)
    (format t " Raw premises: ~s~%" premises)
    (princln " Parsing premises...")
    (setf premise-ulfs (mapcar #'parse-ulf premises))
    (format t " Parsed premises: ~s~%" premise-ulfs)
    (format t "Raw hypothesis: ~s~%" hypothesis)
    (princln " Parsing hypothesis...")
    (setf hypothesis-ulf (parse-ulf hypothesis))
    (format t " Parsed hypothesis: ~s~%" hypothesis-ulf)

    (multiple-value-bind (res infs step-no srcht)
                         (case search-method
                           (:simple-goal-directed (goal-directed-infer-nlog premise-ulfs
                                                                            hypothesis-ulf
                                                                            :max-steps steps
                                                                            :track-sources t))
                           (:heuristic (common-api-heuristic-search premise-ulfs
                                                                    hypothesis-ulf
                                                                    :max-iterations steps
                                                                    :heuristic #'unordered-leaf-f1-new-only-heuristic
                                                                    :track-sources t)))
      (princln "Inference source hash table:")
      (loop for key being the hash-keys of srcht
            using (hash-value value)
            do (format t "Result  ~S:~%  Fn  ~S~%  Args  ~S~%~%"
                       key (first value) (second value)))

      (case res
        (:e (progn
              (format t "Inference ~s ENTAILMENT in ~s steps!~%" id step-no)
              t))
        (:c (progn
              (format t "Inference ~s CONTRADICTION in ~s steps!~%" id step-no)
              t))
        (:u
          (progn
            (format t "Inference ~s UNKNOWN~%" id)
            nil))))))


(defun eval-examples (&key (verbose nil)
                           (filepath "")
                           (sections '())
                           (steps 3)
                           (ids '())
                           (ignore-undefs t))
  "Run FraCaS experiment for given sections."
  (util:memoize 'uci-list)
  (util:memoize 'no2notindef)
  (util:memoize 'quantifier-polarity)
  (util:memoize 'intersective-n+preds)
  (util:memoize 'intersective-noun-modification)
  (util:memoize 'conservativity-inference)
  (util:memoize 'umi-positive-polarity)
  (util:memoize 'umi-negative-polarity)
  (let* (;; Set inference rules.
         ;; NB: Don't use #' for the function names so that we can preserve the
         ;; function names under memoization. #' will directly access the function
         ;; which in the case of memoization is not informative for debugging.
         (*unary-rules* (list 'uci-list
                              'no2notindef
                              'quantifier-polarity
                              'intersective-n+preds
                              'intersective-noun-modification
                              'conservativity-inference
                              'equivalent-quantifier-substitution-inferences
                              ))
         (*binary-rules* (list 'umi-positive-polarity
                               'umi-negative-polarity))
         (*post-inf-normalization-rules* (list 'flatten-n+preds
                                               'collapse-bleached-nominals-in-copulas
                                               'remove-redundant-reification))
         (*initial-ulf-fns* (list 'initial-inferred-polarity-processing))
         ;; Turn natlog memoization on globally.
         (*global-natlog-memo-flag* :on)
         ;; Set package to :ulf-fracas for readability.
         (*package* (find-package :ulf-fracas))
         ;; Set larger-than-default right margin for readability.
         (*print-right-margin* 1000))
    (if verbose
      ;; Verbose evaluation.
      (let* ((json:*json-symbols-package* (find-package :ulf-fracas))
             (jdat (with-open-file (s filepath) (json:decode-json s)))
             (idx 0)
             (filtered (filter-data jdat 'id ids))
             (filtered (filter-data filtered 'group sections))
             (filtered (filter-data filtered 'answer '("yes" "no" "unknown"))))
        (batch-natlog-inputs filtered) ; run natlog in a batch
        (loop for entry in filtered do
              (run-verbose-example entry :steps steps)))
      ;; Compact evaluation.
      (eval-gold :filepath filepath
                 :sections sections
                 :steps steps
                 :ids ids))))

(defun verbose-fracas (&key (filepath "data/fracas.json") (sections '("GQ"))
                                   (steps 2) (ids '()))
  (format t "Running FraCaS experiment for ~s...~%" sections)
  (eval-examples :verbose t
                 :filepath filepath
                 :sections sections
                 :steps steps
                 :ids ids))

