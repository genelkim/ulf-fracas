;; Len's Treebank ULF parser---extended to support some Python-based parsers.

(asdf:defsystem :lenulf+
  :name "lenulf"
  :version "0.0.1"
  :author "Lenhart K. Schubert, packaged and extened by Gene Louis Kim"
  :depends-on (:py4cl :ptb2cf)
  :components ((:file "package")
               (:file "gene-util")
               (:file "english-to-ulf")
               (:file "parse")
               (:file "parse-more")
               (:file "parse-tree-to-ulf")
               (:file "preprocess-tree-for-ulf")
               (:file "pos+word-to-ulf")
               (:file "preprocessing-rules")
               (:file "stem")
               (:file "tt")
               (:file "postprocess-ulf-tree"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    ; NB: debug 3 caused a heap space error.
                    ;(proclaim '(optimize (debug 2) (safety 3) (space 1) (speed 1)))
                    ; For production.
                    (proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next)))

;; This is to store the path to the source code
;; suggested here https://xach.livejournal.com/294639.html
(defpackage #:lenulf+/config (:export #:*base-directory*))
(defparameter lenulf+/config:*base-directory* 
  (asdf:system-source-directory "lenulf+"))

