;; Mandar Juvekar 8-10-2020
;; Data structures for representing ULFs with polarities and utility
;; functions to deal with them.

(in-package :ulf-fracas)

;; A class to mark ULFs with polarities.
(defclass marked-ulf (labeled-tree)
  ((expr
     :initarg :ulf
     :accessor get-ulf)
   (label
     :initarg :global-polarity
     :initform nil
     :accessor global-polarity)))

;; Simple constructor for marked-ulf.
;; Does not propogate polarities (only marks the object created).
;; This function isn't used anywhere right now, but I thought it might be
;; helpful. Might remove it later.
(defun new-marked-ulf (form &key gp)
  (make-instance 'marked-ulf
                 :ulf form
                 :global-polarity gp))

;; Helper function for mark-ulf. Recursively turns ulfs produced by
;; annotate-polarity (in dynamic-polarity.lisp).
(defun mark-helper (ulf)
  (if (and (= (length ulf) 2) (member (second ulf) '(+ - nil o)))
     ; (... +/-/nil)
     (if (atom (car ulf))
       ; (atom +/-/nil)
       (make-instance 'marked-ulf
                      :ulf (car ulf)
                      :global-polarity (second ulf))
       ; ((...) +/-/nil)
       (make-instance 'marked-ulf
                      :ulf (mark-helper (car ulf))
                      :global-polarity (second ulf)))
     ; ((...) (...) ...)
     (mapcar 'mark-helper ulf)))

(defun flip-polarity (pol)
  (cond
    ((eql pol '+) '-)
    ((eql pol '-) '+)
    ((eql pol nil) nil)
    ((eql pol 'o) 'o)
    (t (error "Unknown polarity"))))

;; Use Stanford CoreNLP to mark polarities on a given ULF.
(defun mark-ulf (ulf)
  (if (ttt:match-expr '((!1 not not.adv-s) ((!2 not not.adv-s) _!3))
                      ulf)
    ;; Special handling for (not (not ulf)) since this confuses Stanford CoreNLP.
    (let* ((inner-marked (mark-ulf (second (second ulf))))
           (nmarked (make-instance 'marked-ulf
                                   :ulf (list (make-instance 'marked-ulf
                                                             :ulf 'not
                                                             :global-polarity (global-polarity inner-marked))
                                              inner-marked)
                                   :global-polarity (global-polarity inner-marked)))
           (flippol (flip-polarity (global-polarity inner-marked))))
      (make-instance 'marked-ulf
                     :ulf (list (make-instance 'marked-ulf
                                               :ulf 'not
                                               :global-polarity flippol)
                                nmarked)
                     :global-polarity flippol))
    ;; Default case.
    (mark-helper (annotate-polarity ulf))))

;; Convert a marked ULF to a string.
(defun mulf2string (mulf)
  (if (listp mulf)
    ; ((...) (...) ...)
    (format nil "(~a)"
            (reduce (lambda (x y) (concatenate 'string x " " y))
                    (mapcar 'mulf2string mulf)))
    ; (... +/-/nil)
    (if (atom (get-ulf mulf))
      ; (atom +/-/nil)
      (format nil "~s^~a"
              (get-ulf mulf)
              (global-polarity mulf))
      ; ((...) +/-/nil)
      (format nil "~a^~a"
              (mulf2string (get-ulf mulf))
              (global-polarity mulf)))))

;; Convert a marked ULF to a ULF.
(defun mulf2ulf (mulf)
  (if (listp mulf)
    ; ((...) (...) ...)
    (mapcar 'mulf2ulf mulf)
    (if (atom (get-ulf mulf))
      ; (atom +/-/nil)
      (get-ulf mulf)
      ; ((...) +/-/nil)
      (mulf2ulf (get-ulf mulf)))))

;; Copied from labeled-tree.
(defmethod print-object ((ltree marked-ulf) out)
  (labels
    ((print-helper (curltree)
      (format out "[~s " (global-polarity curltree))
      (cond
        ((listp (get-ulf curltree))
         (format out "(")
         (let ((children (get-ulf curltree)))
           (print-helper (car children))
           (loop for child in (cdr children) do
                 (progn
                   (format out " ")
                   (print-helper child))))
         (format out ")"))
        (t (format out "~s" (get-ulf curltree))))
      (format out "]")))
    (print-unreadable-object (ltree out :type t)
      (print-helper ltree))))

(defun labeled-tree-to-marked-ulf (ltree)
  "Converts from a labeled-tree class to a marked-ulf class."
  (cond
    ((atom (expr ltree))
     (make-instance 'marked-ulf
                    :ulf (expr ltree)
                    :global-polarity (label ltree)))
    (t
     (make-instance 'marked-ulf
                    :ulf (mapcar #'labeled-tree-to-marked-ulf (expr ltree))
                    :global-polarity (label ltree)))))

