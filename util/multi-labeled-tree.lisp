;;; A tree class with multiple labelings, with a base indexing.
;;; This allows labeling lookups based on index.

(in-package :ulf-fracas)

(defclass multi-labeled-tree-node ()
  ((expr
     :initarg :expr
     :accessor expr)
   ;; Indexing for this node.
   (index
     :initarg :index
     :accessor index)
   ;; A list of labels for this node.
   (label-list
     :initarg :label-list
     :accessor label-list)))

(defclass multi-labeled-tree ()
  ((root
     :initarg :root
     :accessor root)
   ;; A list of names for labels in the corresponding index.
   (label-keys
     :initarg :label-keys
     :accessor label-keys)
   ;; Labeled trees
   ;; Individual labeled tree storage to avoid recomputation.
   (labeled-trees
     :initarg :labeled-trees
     :accessor labeled-trees)))

(defmethod print-object ((mlnode multi-labeled-tree-node) out)
  (labels
    ((print-helper (curmlnode)
      (format out "[~s " (index curmlnode))
      (format out "{")
      (when (not (null (label-list curmlnode)))
        (format out "~s" (first (label-list curmlnode))))
      (loop for label in (cdr (label-list curmlnode)) do
            (format out " ~s" label))
      (format out "} ")
      (cond
        ((listp (expr curmlnode))
         (format out "(")
         (let ((children (expr curmlnode)))
           (when (not (null (car children)))
             (print-helper (car children)))
           (loop for child in (cdr children) do
                 (progn
                   (format out " ")
                   (print-helper child))))
         (format out ")"))
        (t (format out "~s" (expr curmlnode))))
      (format out "]")))
    (print-unreadable-object (mlnode out :type t)
      (print-helper mlnode))))

(defmethod print-object ((mltree multi-labeled-tree) out)
  (print-unreadable-object (mltree out :type t)
    (format out "LABEL-KEYS: {")
    (when (not (null (label-keys mltree)))
      (format out "~s" (first (label-keys mltree))))
    (loop for lk in (cdr (label-keys mltree)) do
          (format out " ~s" lk))
    (format out "} ~s" (root mltree))))

(defmethod build-tree-from-node ((mlnode multi-labeled-tree-node)
                                 (mltree multi-labeled-tree))
  "Builds a tree from a multi-labeled-tree-node, mlnode, which is a node of
  a multi-labeled-tree, mltree."
  (make-instance 'multi-labeled-tree
                 :root mlnode
                 :label-keys (label-keys mltree)
                 :labeled-trees
                 (mapcar #'(lambda (lkey) (pull-labeled-tree-at-index
                                            mltree lkey (index mlnode)))
                         (label-keys mltree))))

(defun build-multi-labeled-tree-node (inode lnodes)
  "Builds a multi-labeled-tree-node from an indexed tree and labeled trees."
  (make-instance 'multi-labeled-tree-node
                 :expr
                 (cond
                   ;; Base case.
                   ((atom (expr inode)) (expr inode))
                   ;; Recursive case.
                   (t
                    (mapcar #'build-multi-labeled-tree-node
                            (expr inode)
                            (if lnodes
                              (apply #'zip (mapcar #'expr lnodes))
                              (make-list (length (expr inode)))))))
                 :index (label inode)
                 :label-list (mapcar #'label lnodes)))

(defun build-multi-labeled-tree (itree ltrees label-keys)
  "Builds a multi-labeled-tree from an indexed tree and a list of labeled-trees.

  itree and all ltrees must be labeled-tree classes and ltrees and label-keys
  must be the same length."
  (make-instance 'multi-labeled-tree
                 :root (build-multi-labeled-tree-node itree ltrees)
                 :label-keys label-keys
                 :labeled-trees ltrees))

(defmethod node-at-index ((mlnode multi-labeled-tree-node) index)
  "Gets a node at a given index.

  DFS search."
  (cond
    ;; Found it, return.
    ((eql index (index mlnode)) mlnode)
    ((atom (expr mlnode)) nil)
    (t
     (loop for child in (expr mlnode)
           do (let ((recres (node-at-index child index)))
                (when (not (null recres))
                  (return-from node-at-index recres))))
     ;; Return nil if we didn't find it for any children.
     nil)))

(defmethod node-at-index ((mltree multi-labeled-tree) index)
  "Gets a node at a given index."
  (node-at-index (root mltree) index))

(defmethod get-label-from-key ((mltree multi-labeled-tree) key index)
  "Gets a label for a specific key and index."
  (nth (position key (label-keys mltree))
       (label-list (node-at-index mltree index))))

(defmethod pull-labeled-tree ((mltree multi-labeled-tree) label-key)
  "Pulls a single labeled-tree from a multi-labeled-tree."
  (nth (position label-key (label-keys mltree))
       (labeled-trees mltree)))

(defmethod pull-labeled-tree-at-index ((mltree multi-labeled-tree) label-key index)
  "Pulls a single labeled-tree from a specific index of the tree."
  (labels
    ((find-labeled-subtree-with-index (index iulf lulf)
       "Finds the subtree of a labeling at a given index. Recurse into the
       indexing and labeling concurrently."
       (cond
         ((eql index (index iulf)) lulf)
         ((atom (expr iulf)) nil)
         (t
          (loop for ichild in (expr iulf)
                for lchild in (expr lulf)
                do
                (let ((recres (find-labeled-subtree-with-index index
                                                               ichild
                                                               lchild)))
                  (when recres
                    (return-from find-labeled-subtree-with-index
                                 recres))))))))
    (let ((key-pos (position label-key (label-keys mltree))))
      (find-labeled-subtree-with-index
        index
        (root mltree)
        (nth key-pos (labeled-trees mltree))))))

(defmethod pull-index-tree ((mlnode multi-labeled-tree-node))
  (cond
    ((atom (expr mlnode))
     (make-instance 'labeled-tree
                    :expr (expr mlnode)
                    :label (index mlnode)))
    (t
     (make-instance 'labeled-tree
                    :expr (mapcar 'pull-index-tree
                                  (expr mlnode))
                    :label (index mlnode)))))

(defmethod pull-index-tree ((mltree multi-labeled-tree))
  (pull-index-tree (root mltree)))

(defmethod pull-index-tree-at-index ((mltree multi-labeled-tree) index)
  "Pulls a single labeled-tree corresponding to the indexing starting at a
  specific index."
  (pull-index-tree (node-at-index mltree index)))


(defmethod leaves ((mlnode multi-labeled-tree-node))
  "Pulls out a list of leave nodes."
  (cond
    ((atom (expr mlnode)) (list mlnode))
    (t (apply #'append (mapcar #'leaves (expr mlnode))))))

(defmethod leaves ((mltree multi-labeled-tree))
  (leaves (root mltree)))

