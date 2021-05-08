;; Len's Treebank to ULF parser
;; Packaged on 2020-10-01

(asdf:defsystem :lenulf
  :name "lenulf"
  :version "0.0.1"
  :author "Lenhart K. Schubert, packaged by Gene Louis Kim"
  :components ((:file "package")
               (:file "gene-util")
               (:file "english-to-ulf")
               (:file "parse")
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
