
(in-package :ulf-fracas)

;; A general class for constructing a labeled trees without internal values
;; recursively. Each node of the tree is labeled but only the leaf nodes have
;; values. This reflects a labeled ULF. For more general tree structures,
;; something like [functional
;; trees](https://quickref.common-lisp.net/functional-trees.html) is probably
;; best.
(defclass labeled-tree ()
  ((expr
     :initarg :expr
     :accessor expr)
   (label
     :initarg :label
     :accessor label)))

(defmethod print-object ((ltree labeled-tree) out)
  (labels
    ((print-helper (curltree)
      (format out "[~s " (label curltree))
      (cond
        ((listp (expr curltree))
         (format out "(")
         (let ((children (expr curltree)))
           (print-helper (car children))
           (loop for child in (cdr children) do
                 (progn
                   (format out " ")
                   (print-helper child))))
         (format out ")"))
        (t (format out "~s" (expr curltree))))
      (format out "]")))
    (print-unreadable-object (ltree out :type t)
      (print-helper ltree))))

(defun dfs-label-tree (tree)
  "Returns a DFS, 0-indexed labeled tree."
  (labels
    ((rechelper (curtree idx)
       "A recursive helper function. Keeps track of the current index and
       returns the new index alongside the labeled tree."
       (cond
         ;; Base case 1 (leaf node).
         ((atom curtree) (values (make-instance 'labeled-tree
                                                :expr curtree
                                                :label idx)
                                 (1+ idx)))
         ;; Recrusive case (internal node).
         (t (let ((curidx (1+ idx))
                  ltrees)
              (setf ltrees
                    (loop for child in curtree collect
                          (multiple-value-bind
                            (newltree newidx)
                            (rechelper child curidx)
                            (setf curidx newidx)
                            newltree)))
              (values (make-instance 'labeled-tree
                                     :expr ltrees
                                     :label idx)
                      curidx)))))) ; end of labels defs.
    ;; Labels body.
    (rechelper tree 0)))

(defmethod unlabeled-tree ((ltree labeled-tree))
  (cond
    ((atom (expr ltree)) (expr ltree))
    (t (mapcar 'unlabeled-tree (expr ltree)))))

(defmethod get-node-at-label ((ltree labeled-tree) label)
  "Assuming a unique labeling of nodes, returns the node associated with the
  given label."
  (cond
    ((eql label (label ltree)) ltree)
    ((atom (expr ltree)) nil)
    (t (loop for child in (expr ltree)
             do (let ((recres (get-node-at-label child label)))
                  (when recres (return-from get-node-at-label recres)))))))

(defmethod replace-expr-at-label ((ltree labeled-tree) label expr)
  "Returns the same labeled-tree but with the expression of the node with the
  given label replaced with `expr`. This function makes no effort to ensure
  that the labels in expr don't overlap with the rest or even that it recurses
  properly into further labeled-trees."
  (cond
    ;; Found it
    ((eql label (label ltree))
     (make-instance 'labeled-tree
                    :expr expr
                    :label label))
    ;; Leaf node
    ((atom (expr ltree)) ltree)
    ;; Recurse
    (t
     (make-instance 'labeled-tree
                    :expr (mapcar #'(lambda (child)
                                      (replace-expr-at-label child label expr))
                                  (expr ltree))
                    :label (label ltree)))))

(defun create-label-map (ltree1 ltree2 &key (test #'equal))
  "Given two labeled trees of the same shape, creates a hash table from the
  labels of the first tree to the labels of the second tree. This assumes that
  the labels of the first argument, ltree1, are unique. Otherwise, an arbitrary
  mapping will be selected.

  Key argument `test` is the function for testing equality between keys."
  (labels
    ((rechelper (lt1 lt2 ht)
       "Recursive helper function with the hash table as an argument."
       (let ((e1 (expr lt1))
             (e2 (expr lt2))
             (l1 (label lt1))
             (l2 (label lt2)))
         ;; Trees not the same shape, error.
         (when (or (and (atom e1) (not (atom e2)))
                   (and (not (atom e1)) (atom e2)))
           (error "Cannot create a label map for trees of different shapes."))
         ;; Add current label mapping.
         (setf (gethash l1 ht) l2)
         ;; Non-leaf, recurse.
         (when (and (listp e1) (listp e2) (= (length e1) (length e2)))
           (mapcar #'(lambda (c1 c2) (rechelper c1 c2 ht))
                   e1 e2))
         ;; Return the hash table.
         ht)))
    ;; Main body of labels, call the recursive helper function with initial
    ;; conditions.
    (rechelper ltree1 ltree2 (make-hash-table :test test))))

(defun apply-label-map (ltree lmap)
  "Constructs a new labeled tree after applying a label map.

  ltree: A labeled tree.
  lmap: A hash table from the current labeled tree to a new labeling."
  (cond
    ((atom (expr ltree))
     (make-instance 'labeled-tree
                    :expr (expr ltree)
                    :label (gethash (label ltree) lmap)))
    (t
     (make-instance 'labeled-tree
                    :expr (mapcar #'(lambda (child)
                                      (apply-label-map child lmap))
                                  (expr ltree))
                    :label (gethash (label ltree) lmap)))))

(defun find-divergences (lulf1 lulf2)
  "Searches the two index-labeled ULFs to find a list of points where the
  subtree values diverge. Returns a list of pairs (cons cell) of indices."
  (let ((e1 (expr lulf1))
        (e2 (expr lulf2))
        (l1 (label lulf1))
        (l2 (label lulf2))
        recres childlabels)
    (cond
      ;; Base case 1, one is atomic and not equal to the other.
      ((and (or (atom e1) (atom e2))
            (not (eql e1 e2)))
       (list (cons l1 l2)))
      ;; Base case 2, both are atomic and equal.
      ((and (atom e1) (atom e2) (eql e2 e2))
       nil)
      ;; Base case 3, both are lists, but not the same length.
      ((not (= (length e1) (length e2)))
       (list (cons l1 l2)))
      ;; Recursive case, bot are lists and the same length.
      ;; Recurse into all children. If all children are different just put
      ;; this not as the divergence.
      (t
       (setf recres (mapcar #'find-divergences e1 e2))
       (if (every #'assoc (mapcar 'label e1) recres)
         ;; All children are divergent, so just use this node.
         (list (cons l1 l2))
         ;; Not all children are divergent, so merge their results.
         (apply #'append recres))))))

