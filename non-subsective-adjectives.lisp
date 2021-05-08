;;; Computes a hash table of non-subsective adjectives and provides a function
;;; for checking if a ULF adjective is non-subsective.

(in-package :ulf-fracas)

(defparameter *non-subsective-adjective-ht* (make-hash-table :test #'equal))

(defun fill-non-subsective-adjective-ht ()
  (let* ((json:*json-symbols-package* (find-package :ulf-fracas))
         (jdat (with-open-file (s *non-subsective-adjective-file*)
                 (json:decode-json s))))
    (loop for entry in (cdrassoc 'data jdat) do
          (let* ((word (cdrassoc 'word entry))
                 (senses (cdrassoc 'wn_senses entry))
                 (synsets (synsets-for-word word)))
            ;; Add word to hash table.
            (setf (gethash (intern (string-upcase word))
                           *non-subsective-adjective-ht*)
                  t)
            ;; Add other members of synsets to hash table.
            (loop for wnsense in senses do
                  (let ((synset-words
                          (mapcar #'first (nth (1- wnsense) synsets))))
                    (loop for sword in synset-words do
                          (setf (gethash (intern (string-upcase sword))
                                         *non-subsective-adjective-ht*)
                                t))))))))

;; Fill non-subsective-adj-ht.
(fill-non-subsective-adjective-ht)

(defun non-subsective-adj? (ulf)
  (and (atom ulf)
       (adj? ulf)
       (gethash (split-by-suffix ulf) *non-subsective-adjective-ht*)))

