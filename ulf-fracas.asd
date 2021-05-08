;; ULF Natural Logic
;; Packaged on 2020-6-22

(asdf:defsystem :ulf-fracas
  :name "ulf-fracas"
	:serial t
  :version "1.0.0"
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>, Mandar Juvekar, Junis Ekmekciu, Viet Duong"
  :license "MIT"
  :depends-on (:ttt :cl-strings :cl-json :cl-util :cl-ppcre :ulf-lib
               :ulf2english :cl-ansi-text :uiop :py4cl :lenulf+ :standardize-ulf :fset
               :priority-queue :wordnet :bordeaux-threads :cl-progress-bar)
  :components ((:file "package")
               (:file "python-manager")
               (:file "util/util")
               (:file "util/labeled-tree")
               (:file "util/multi-labeled-tree")
               (:file "util/wordnet")
               (:file "dynamic-polarity/dynamic-polarity")
               (:file "dynamic-polarity/marked-ulf")
               (:file "dynamic-polarity/inferred-polarity")
               (:file "non-subsective-adjectives")
               (:file "inference-nlog")
               (:file "heuristics")
               (:file "inference")
               (:file "quantifier-inferences")
               (:file "conservativity-inference")
               (:file "extract-monotonicity-rules")
               (:file "parse-ulf")
               (:file "post-inference-normalization")
               (:file "batch-process")
               (:file "evaluation/aristotle")
               (:file "evaluation/fracas"))
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (space 1) (speed 1)))
                    (funcall next))
  :in-order-to ((test-op (test-op :ulf-fracas/tests))))

(asdf:defsystem :ulf-fracas/tests
	:serial t
	:description "Unit tests for the ulf-fracas library"
	:author "Gene Louis Kim <gkim21@cs.rochester.edu>, Mandar Juvekar, Junis Ekmekciu, Viet Duong"
  :license "MIT"
  :depends-on (:ulf-fracas :lisp-unit :cl-util)
  :pathname #P"test/"
  :components ((:file "package")
               (:file "test-util")
               ; Inference tests..
               (:file "uci")
               (:file "umi")
               (:file "quantifier-inferences")
               (:file "conservativity-inference")
               (:file "extract-monotonicity-rules")
               ; Aristotelian tests.
               (:file "aristotle")
               (:file "dynamic-polarity")
               (:file "parse-ulf")
               (:file "post-inference-normalization")
               ; Inferred polarity.
               (:file "inferred-polarity")
               ; Engine tests
               (:file "nlog-engine-tests")
               ; Simple fracas tests
               (:file "fracas"))
  :perform (test-op (o c) (symbol-call :ulf-fracas/tests :run)))

