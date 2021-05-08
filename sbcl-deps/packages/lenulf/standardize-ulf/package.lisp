;; ULF standardizer for Len's Treebank to ULF parser.

(in-package :cl-user)

(defpackage :standardize-ulf
  (:use :cl :cl-user :ttt :cl-strings :ulf-lib :cl-util :ulf2english)
  (:import-from :py4cl :python-eval)
  (:shadow :insert)
  (:shadowing-import-from :cl-util #:compose)
  (:shadowing-import-from :cl-strings #:join)
  (:export :standardize-ulf))
