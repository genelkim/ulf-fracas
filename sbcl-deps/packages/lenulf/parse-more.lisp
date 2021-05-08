;; Code that implements the K&K and K&M parsers.

(in-package :lenulf)

;; Flag for loading the K&K parser if necessary.
(defparameter *k&k-setup-complete* nil)
;; Flag for loading the K&M parser if necessary.
(defparameter *k&m-setup-complete* nil)
(defparameter *k&m-path* (merge-pathnames "deps/self-attentive-parser/src/"
                                          lenulf+/config:*base-directory*))
(defparameter *k&m-pretrained-model*
  (merge-pathnames "deps/model/model-BERT_dev=94.81.pt"
                   lenulf+/config:*base-directory*))
(defparameter *k&m-dict* (merge-pathnames "deps/model/dict"
                                          lenulf+/config:*base-directory*))

(rewrite parse-kk (str)
;; Calls the standard K&K parser through python.
    (when (not *k&k-setup-complete*)
      (format t "Loading K&K parser...")
      (finish-output)
      (py4cl:python-exec "import benepar")
      (py4cl:python-exec "benepar.download('benepar_en3')")
      (py4cl:python-exec "benepar_parser = benepar.Parser('benepar_en3')")
      (setf *k&k-setup-complete* t)
      (format t "Done!~%"))
    (lispify-parser-output
      (py4cl:python-eval (format nil "str(benepar_parser.parse(\"~a\"))" str))))

(rewrite parse-km (str)
;; Calls the pretrained K&K parser for K&M through python. This is slightly
;; more complicated than the standard K&K parser since it isn't part of the
;; benepar package. Then post-processes it with the Lisp package for recovering
;; the PTB format. This function is independently implemented since it's
;; complicated enough already.
    (when (not *k&m-setup-complete*)
      (format t "Loading K&M parser...")
      (finish-output) ; flush.
      ;; Set up nltk tokenizer.
      (py4cl:python-exec "import nltk")
      (py4cl:python-exec "nltk.download('punkt')")
      (py4cl:python-exec "from nltk.tokenize import word_tokenize")
      ;; Add the parser code to the system path (since it isn't packaged and installed).
      (py4cl:python-exec "import sys")
      (py4cl:python-exec (format nil "sys.path.append(\"~a\")" *k&m-path*))
      ;; Load up parser.
      (py4cl:python-exec "import torch")
      (py4cl:python-exec "import parse_nk")
      (py4cl:python-exec (format nil
                                 "info = (torch.load(\"~a\") if torch.cuda.is_available() else torch.load(\"~a\", map_location=lambda storage, location: storage))"
                                 *k&m-pretrained-model*
                                 *k&m-pretrained-model*))
      (py4cl:python-exec "parser = parse_nk.NKChartParser.from_spec(info['spec'], info['state_dict'])")
      (setf *k&m-setup-complete* t)
      (format t "Done!~%"))

    ;; Parse sentence.
    (py4cl:python-exec (format nil "tokens = word_tokenize(\"~a\")" str))
    (py4cl:python-exec "predicted, _ = parser.parse([('UNK', token) for token in tokens])")
    (let ((pyout (py4cl:python-eval "predicted.convert().linearize()"))
          (mid-file (format nil "~a.txt" (gensym)))
          (end-file (format nil "~a.txt" (gensym)))
          result)
      ;; TODO: speed this up by calling the cf->ptb internal code directly rather than through the file system.
      (with-open-file (to-recover mid-file :direction :output
                                           :if-exists :supersede)
        (format to-recover pyout))
      (ptb2cf:cf->ptb :cf mid-file :ptb end-file :dictionary *k&m-dict*)
      (setf result (lispify-parser-output
                     (with-open-file (s end-file)
                       (let ((data (make-string (file-length s))))
                         (read-sequence data s)
                         data))))
      (delete-file mid-file)
      (delete-file end-file)
      result))

