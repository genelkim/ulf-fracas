;;; Gene's custom utilities for this package. For post-processing or additional
;;; formatting options.

(in-package :lenulf)

(defun remove-token-index (idxsym &key (delim #\~))
  "Removes the indexing from a symbol token, assuming the indexing is provided
  with a delimiter at the end of the symbol. Default delimiter, tilde."
  (let ((symstr (symbol-name idxsym)))
    (intern (subseq symstr 0 (position delim symstr :from-end t))
            :lenulf)))

(defun remove-token-indices (idxulf &key (delim #\~))
  "Removes the indexings from a ulf, assuming the indexing is provided with a
  delimiter at the end of the symbol. Default delimiter, tilde."
  (cond
    ((null idxulf) idxulf)
    ((atom idxulf) (remove-token-index idxulf :delim delim))
    (t (mapcar #'(lambda (x) (remove-token-indices x :delim delim))
               idxulf))))

;; Function redefinition from https://stackoverflow.com/a/1220229
;; This is used for the simple and extended versions of the package. The macro
;; `define` defines a function while remembering the source. The function
;; `modify` modifies the source code for the function defined by the `define`
;; macro.
;; `rewrite` overwrite the function with a new source.
(defmacro define (&rest source)
  `(progn (setf (get ',(first source) :source) (list* 'defun ',source))
     (defun ,@source)))

(defun modify (fname modifier)
  (let ((source (get fname :source)))
    (when source
      (setf (get fname :source) (funcall modifier source))
      (eval (get fname :source))
      (compile fname))))

(defmacro rewrite (&rest newsource)
  `(let ((fname ',(first newsource)))
;      (oldsource (get fname :source)))
;    (format t "oldsource: ~s~%" oldsource)
;    (format t "fname: ~s~%" fname)
;    (format t "newsource: ~s~%" ',newsource)
;    (error "Temp failure.")))
     (setf (get fname :source) (list* 'defun ',newsource))
     (eval (get fname :source))
     (compile fname)))

