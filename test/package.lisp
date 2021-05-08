(defpackage #:ulf-fracas/tests
    (:use #:cl #:lisp-unit #:ulf-fracas #:cl-util)
      (:export #:run))

(in-package :ulf-fracas/tests)

(defun run (&key tests tags
                 ;; lisp-unit verbosity parameters.
                 (print-failures t)
                 (print-errors t)
                 (print-summary t)
                 (summarize-results t))
  "Run all ulf-fracas unit tests.
  
  Optional arguments:
    tests:  list of test names, defaults to running all tests
    tags:   list of test tags

  `tests` and `tags` should not both be set. The `tags` argument will be
  ignored if that is the case.
  "
  (let ((*print-failures* print-failures)
        (*print-errors* print-errors)
        (*print-summary* print-summary)
        (*summarize-results* summarize-results))
    ;; Run tests.
    (cond
      ;; Specified tests.
      (tests
        (when tags
          (warn (concatenate 'string
                             "Both the :tags and :tests keyword "
                             "arguments are given for ulf-fracas/tests:run. "
                             "Ignoring the :tags argument...")))
        (util:in-intern (tests pkgtests :ulf-fracas/tests)
          (run-tests pkgtests :ulf-fracas/tests)))
      ;; Specified tags.
      (tags (run-tags tags :ulf-fracas/tests))
      ;; Default, all tests.
      (t (run-tests :all :ulf-fracas/tests)))))

