
(in-package :ulf-fracas)

(defun zip (&rest rest)
  (if rest
    (apply #'mapcar (cons #'list rest))
    nil))

(defun cdrassoc (key alist) (cdr (assoc key alist)))

(defun filter-data (data key values)
  (if (or (null key) (null values))
    data
    (if (stringp (car values))
      (remove-if-not #'(lambda (entry) (member (cdrassoc key entry) values :test #'string=))
                     data)
      (remove-if-not #'(lambda (entry) (member (cdrassoc key entry) values :test #'equal))
                     data))))

(defun get-answer (res)
  (cond
    ((eq res :e) "yes")
    ((eq res :c) "no")
    (t "unknown")))

;; Macros for semantically marking statements that should be colored.
(defmacro sysmsg (&body body)
  `(with-color (:cyan)
     ,@body))
(defmacro syspas (&body body)
  `(with-color (:green)
     ,@body))
(defmacro syswarn (&body body)
  `(with-color (:yellow)
     ,@body))
(defmacro syserr (&body body)
  `(with-color (:red)
     ,@body))
     
(defun find-expr (expr lulf)
  "Finds the subtree of lulf that corresponds to expr.
  Returns nil if not found."
  (labels
    ((rechelper (lu u)
       "Recursieve helper function to avoid redundant unlabeled-tree function
       calls."
       (cond
         ;; Base case, fouund it.
         ((equal expr u) lu)
         ;; Base case, not found.
         ((atom u) nil)
         ;; Recursive case.
         (t
           (loop for lchild in (expr lu)
                 for uchild in u
                 do
                 (let ((recres (rechelper lchild uchild)))
                   (when (not (null recres))
                     ;; Found it
                     (return-from rechelper recres))))))))
    ;; Body of flet.
    (let ((ulf (unlabeled-tree lulf)))
      (rechelper lulf ulf))))

(defun find-all-expr (expr lulf)
  "Finds all subtrees of lulf that corresponds to expr.
  Returns nil if none is found."
  (labels
    ((rechelper (lu u)
       "Recursieve helper function to avoid redundant unlabeled-tree function
       calls."
       (cond
         ;; Base case, fouund it.
         ((equal expr u) (list lu))
         ;; Base case, not found.
         ((atom u) nil)
         ;; Recursive case.
         (t
           (apply #'append
                  (loop for lchild in (expr lu)
                        for uchild in u
                        collect (rechelper lchild uchild)))))))
    ;; Body of flet.
    (let ((ulf (unlabeled-tree lulf)))
      (rechelper lulf ulf))))

(defun flip-lulf-polarity (pulf)
  "Flips polarity of polarity labeled tree."
  (let ((new-label (flip-polarity (label pulf))))
    (cond
      ((atom (expr pulf))
       (make-instance 'labeled-tree
                      :expr (expr pulf)
                      :label new-label))
      (t
       (make-instance 'labeled-tree
                      :expr (mapcar #'flip-lulf-polarity (expr pulf))
                      :label new-label)))))

