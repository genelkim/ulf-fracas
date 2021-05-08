;; ULF Natural Logic
;; Packaged on 2020-6-22

(in-package :cl-user)

(defpackage :ulf-fracas
  (:use :cl :ttt :cl-strings :cl-json :ulf-lib :lisp-unit :cl-util :ulf2english :standardize-ulf :wordnet)
  (:import-from :py4cl :python-eval)
  (:import-from :cl-ansi-text :with-color)
  (:shadow :insert)
	(:shadowing-import-from :cl-util #:compose)
  (:shadowing-import-from :cl-strings #:join)
  (:export
    ;; ULF Conversion
    is-conversion-form
    conversion-rule
    conversion-rule-ttt
    ;; UMI
    mon-form?
    umi-positive-polarity
    umi-negative-polarity
    ;; Quantifier
    quantifier-polarity
    extract-monotonicity-rules
    ;; Inference engine
    infer-nlog
    infer-nlog
    goal-directed-infer-nlog
    goal-directed-infer-nlog))

;; Global variables.
(in-package :ulf-fracas)
(defparameter *debug-ulf-fracas* nil)
(defparameter *dynamic-polarity-dir* "dynamic-polarity")
(defparameter *non-subsective-adjective-file* "resources/nayak_nonsubsective.json")
;; Flag to globally control the memoization behavior of natlog calls.
;; *global-natlog-memo-flag* can be :on, :off, or :local.
;;  :on always memoize
;;  :off never memoize
;;  :local use the local argument
(defparameter *global-natlog-memo-flag* :local)

;; Make lenulf silent.
(setf lenulf::*show-stages* nil)

