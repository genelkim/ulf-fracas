;;; Batch processing rules.

(in-package :ulf-fracas)

(defun batch-natlog-ulf-fset (ulfset)
  "Runs an fset of ULFs through ulf-to-string and then run-natlog in a batch so
  we minimize the overhead of spinning up the JVM. This is memoized so that later
  memoized calls to run-natlog will be a simple lookup.

  NB: Make sure the global memoization settings aren't turning it off."
  (when (eql *global-natlog-memo-flag* :off)
    (warn (concatenate
            'string
            "The global memoization flag (*global-natlog-memo-flag*) is set "
            "to :off. Please set of :on or :local for the batch natlog "
            "processing to have its intended effect.")))
  (run-natlog-ulfs (fset:convert 'list ulfset) :memoize t)
  ;; Just return the input values. We're just running this to get the processed
  ;; values in the memo.
  ulfset)

