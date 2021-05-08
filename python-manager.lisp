;;; Manages the python environments for the different functions.
;;; This allows the simultaneous use of Python 2 and 3. Also
;;; avoids the different uses of Python from overwriting each
;;; other.

(in-package :ulf-fracas)

;; Hash table from environment name to python command.
(defparameter *python-command-ht*
  (alexandria:alist-hash-table
    ;; The simplest installation puts both in the same python environment. But
    ;; the URCS cluster doesn't support a dependency needed for Python 3 usage
    ;; of ulf2english.
    ;'((ulf2english . "python2") ; ulf2english
    '((ulf2english . "python3") ; ulf2english
      (benepar . "python3")))) ; Berkeley parser.

;; Hash table from environment name to python process.
(defparameter *python-process-ht* (make-hash-table :test #'eql))
;; Hash table for localized ulf2english setup-complete parameter.
(defparameter *ulf2english-setup-ht* (make-hash-table :test #'eql))

(defmacro with-python-env ((env) &body body)
  "Runs the body under the given python environment name."
  `(let ((py4cl::*python-command* (gethash ,env *python-command-ht*))
         (py4cl::*python* (gethash ,env *python-process-ht*))
         (ulf2english::*setup-complete* (gethash ,env *ulf2english-setup-ht*))
         result)
     (setf result (multiple-value-list (progn ,@body)))
     (setf (gethash ,env *python-process-ht*) py4cl::*python*)
     (setf (gethash ,env *ulf2english-setup-ht*) ulf2english::*setup-complete*)
     (values-list result)))

(defun python-exec-env (cmd env)
  "Calls py4cl:python-exec with the given internal environment."
  (with-python-env (env) (py4cl:python-exec cmd)))

(defun python-eval-env (cmd env)
  "Calls py4cl:python-eval with the given internal environment."
  (with-python-env (env) (py4cl:python-eval cmd)))

