;;; Utilities to simplify wordnet operations.
;;; Uses a common lisp interface provided by https://github.com/phoe/wordnet
;;; It is directly accessible via quicklisp

(in-package :ulf-fracas)

(defparameter +part-of-speech-table+
  (remove-duplicates
    (mapcar #'third wordnet::+part-of-speech-table+)))

(defun synsets-for-word (wrd &optional pos)
  "Returns the WordNet synsets for the word.

  May optionally provide a part-of-speech restriction as a keyword argument.
  Example:
    (synsets-for-word \"dog\")
    -> ..
    (synsets-for-word \"dog\" :noun)
    -> .."
  (let ((poslst (if pos
                 (list pos)
                 +part-of-speech-table+)))
    (apply #'append
           (loop for curpos in poslst
                 collect (index-entry-synsets
                           (cached-index-lookup wrd curpos))))))

(defmethod hypernyms ((synset wordnet-synset-entry))
  "Returns a list of hypernym synsets."
  (let ((pointers (wordnet-pointers synset)))
    (mapcar #'wordnet-pointer-to-synset
            (remove-if-not #'(lambda (ptr) (eql :hypernym
                                                (wordnet-pointer-type ptr)))
                           pointers))))

(defmethod ancestors ((synset wordnet-synset-entry))
  "Returns a list of ancestor synsets."
  (let ((hypers (hypernyms synset)))
    (apply #'append
           (cons hypers
                 (mapcar #'ancestors
                         hypers)))))

(defun ancestor-words (refword &optional pos)
  (let* ((ref-synsets (synsets-for-word refword pos))
         (ancestors (apply #'append (mapcar #'ancestors ref-synsets))))
    (remove-duplicates
      (apply #'append
             (mapcar #'(lambda (anc)
                         (mapcar #'first (synset-words anc)))
                     ancestors)))))

(defun is-wordnet-ancestor-of (newword refword &optional pos)
  "Returns whether `newword` is an ancestor of `refword` in WordNet.

  May optionally provide the part of speech restriction on the word."
  (member newword (ancestor-words refword pos) :test #'equal))

