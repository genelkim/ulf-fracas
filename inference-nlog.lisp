;; Mandar Juvekar 8-12-2020
;; Inference engine for ULF natural logic inferences

(in-package :ulf-fracas)

; Rules take X ulfs as params -> return list of inferences
(defparameter *unary-rules* '())
(defparameter *binary-rules* '())
(defparameter *post-inf-normalization-rules* '())
(defparameter *batch-process-new-fns* '())
(defparameter *initial-ulf-fns* '())

(defparameter *step-cost* short-float-epsilon)

;; Given two lists get all ordered pairs in the cartesian product a x b. STALE
(defun get-pairs (a b)
  (reduce 'append
    (mapcar (lambda (x)
              (mapcar (lambda (y)
                       (list x y))
                      b))
              a)))

;; Given two sets get a set of all ordered pairs in the cartesian product a x b.
(defun get-set-pairs (a b)
  (fset:reduce #'fset:union
          (fset:image (lambda (x)
                        (fset:image (lambda (y) (list x y))
                                    b))
                      a)
          :initial-value (fset:empty-set)))

;; Builds the unary function/input pairs.
(defun build-unary-pairs (forms)
  (get-set-pairs (fset:convert 'fset:set *unary-rules*)
                 (fset:image #'list forms)))

;; Builds the new binary function/input pairs. Given the new formulas and
;; old formulas, only ones that haven't been explored before are built.
(defun build-binary-pairs (new old)
  (get-set-pairs (fset:convert 'fset:set *binary-rules*)
                 (fset:union (get-set-pairs new new)
                             (fset:union (get-set-pairs old new)
                                         (get-set-pairs new old)))))

;; Given a list of function/input pairs and a set of old formulas, returns
;; a set of new formulas inferred from the pairs.
;; `old-sources` should be a fset:map with the old source mappings. If this
;; is nil, the return value sources will be empty.
(defun compute-new-results (pairs old &key old-sources)
  (if (fset:empty? pairs)
    (values (fset:empty-set) (fset:empty-map))
    (let ((sources
            (fset:reduce
              #'fset:map-union
              (fset:image (lambda (pair)
                            (let ((fn (first pair))
                                  (args (second pair)))
                              (fset:convert 'fset:map
                                            (mapcar #'(lambda (res)
                                                        (cons (postinf-normalize res)
                                                              pair))
                                                    (apply fn args)))))
                          pairs))))
      (values (fset:set-difference (fset:domain sources) old)
              (if old-sources
                (fset:map-difference-2 sources old-sources)
                (fset:empty-map))))))

;; Calls each normalization function in a row.
(defun postinf-normalize (ulf)
  (reduce (lambda (acc fn) (funcall fn acc))
          *post-inf-normalization-rules*
          :initial-value ulf))

;; Calls the batch processing rules.
(defun batch-process-new (ulfset)
  (reduce (lambda (cur-ulfset fn) (funcall fn cur-ulfset))
          *batch-process-new-fns*
          :initial-value ulfset))

;; Calls the initialization ULF processing rules.
;; Does not return a value.
(defun initial-ulf-processing (ulflst goal)
  (let ((curulfs ulflst)
        (curgoal goal))
    (mapcar #'(lambda (fn) (multiple-value-bind
                             (newulfs newgoal)
                             (funcall fn curulfs curgoal)
                             (setf curulfs newulfs)
                             (setf curgoal newgoal)))
            *initial-ulf-fns*)
    (values curulfs curgoal)))

;; The main ULF-NLOG forward inference function. Exhaustively applies inference
;; rules to the given ulfs for a given number of steps and outputs all ulfs
;; discovered.
;; track-sources is a flag to keep track of sources of inferences in a hash
;; table, which is returned as a value. If track-sources is nil, an empty hash
;; table is returned.
(defun infer-nlog (in-ulfs nsteps &key callpkg track-sources)
  (declare (optimize (debug 3)))
  (inout-intern (in-ulfs ulfs :ulf-fracas :callpkg callpkg)
    ; maintain a list of discovered ulfs and a hashtable of inferences carried out
    (let ((sources (if track-sources (fset:empty-map) nil))
          (new (fset:convert 'fset:set ulfs))
          (old (fset:empty-set)))
      (loop for n from 1 to nsteps do
            ; look at all pairs of unary rules and discovered ulfs and all pairs
            ; of binary rules and pairs of ulfs, except those already inferred
            (let ((unary-pairs (build-unary-pairs new))
                  (binary-pairs (build-binary-pairs new old))
                  unary-results binary-results)
              ; Batch process new formulas (e.g. run Stanford CoreNLP with minimal overhead).
              (setf new (batch-process-new new))
              (setf old (fset:union old new))
              ; Compute new results
              (setf unary-results (multiple-value-list
                                    (compute-new-results unary-pairs
                                                         old
                                                         :old-sources sources)))
              (setf binary-results (multiple-value-list
                                     (compute-new-results binary-pairs
                                                          old
                                                          :old-sources sources)))
              ; Gather and normalize new inferences.
              ;(setf new (fset:image #'postinf-normalize ; This is now done during computation
              (setf new (fset:union (first unary-results)
                                    (first binary-results)))

              ; Track sources
              (when track-sources
                (setf sources (fset:reduce
                                #'fset:map-union
                                (list (second binary-results)
                                      (second unary-results)
                                      sources))))))

      ; return
      (values (fset:convert 'list (fset:union new old))
              (if track-sources
                (fset:convert 'hash-table sources)
                (make-hash-table :test #'equal))))))

;; A simple ULF-NLOG goal-directed inference function. Exhaustively apply
;; interence rules to the given ulfs until the goal or its contradiction is
;; inferred, or until a given maximum number of steps is reached (keyword
;; parameter). Returns a values list with
;;  - :e if entailed, :c if contradicted, and :u if not found
;;  - a list of all ulfs inferred,
;;  - the number of steps searched, and
;;  - a hash table of the inference sources (or nil if track-sources is nil).
;; track-sources is a flag to keep track of sources of inferences in a hash
;; table, which is returned as a value. If track-sources is nil, an empty hash
;; table is returned.
(defun goal-directed-infer-nlog (in-ulfs in-goal &key max-steps callpkg track-sources)
  (declare (optimize (debug 3)))
  (inout-intern (in-ulfs ulfs :ulf-fracas :callpkg callpkg)
    (in-intern (in-goal goal :ulf-fracas)
    ;; Initial processing of ULFs.
    (multiple-value-bind (newulfs newgoal) (initial-ulf-processing ulfs goal)
      (setf ulfs newulfs)
      (setf goal newgoal))
    ; maintain a list of discovered ulfs and a hashtable of inferences carried out
    (let ((sources (if track-sources (fset:empty-map) nil))
          (new (fset:convert 'fset:set ulfs))
          (old (fset:empty-set))
          (step-no 0)
          (neggoal (list 'not goal)))
      (loop
            ; look at all pairs of unary rules and discovered ulfs and all pairs
            ; of binary rules and pairs of ulfs, except those already inferred
            (let ((unary-pairs (build-unary-pairs new))
                  (binary-pairs (build-binary-pairs new old))
                  unary-results binary-results)
              ; Batch process new formulas (e.g. run Stanford CoreNLP with minimal overhead).
              (setf new (batch-process-new new))

              (format t "new inferences~%")
              (loop for inf in (fset:convert 'list new)
                    do (format t "  ~s~%" inf))
              (format t "~%~%~%")

              (setf old (fset:union old new))

              ; Compute new results
              (setf unary-results (multiple-value-list
                                    (compute-new-results unary-pairs
                                                         old
                                                         :old-sources sources)))
              (setf binary-results (multiple-value-list
                                     (compute-new-results binary-pairs
                                                          old
                                                          :old-sources sources)))
              (format t "Gathering and normalizing new inferences.~%")
              ; Gather and normalize new inferences.
              (setf new (fset:image #'postinf-normalize
                                    (fset:union (first unary-results)
                                                (first binary-results))))

              ; Track sources
              (when track-sources
                (setf sources (fset:reduce
                                #'fset:map-union
                                (list (second binary-results)
                                      (second unary-results)
                                      sources))))
              (setf *infer-nlog-sources* (fset:convert 'hash-table sources :test #'equal))

              ; Check if the goal has been found, and if so return
              (format t "Checking if goal is found.~%")
              (when (fset:contains? new goal)
                (return-from goal-directed-infer-nlog
                             (values :e
                                (fset:convert 'list (fset:union new old))
                                step-no
                                (if track-sources
                                  (fset:convert 'hash-table sources)
                                  (make-hash-table :test #'equal)))))

              ; Check if the contradiction has been found, and if so return
              (format t "Checking if contradiction is found.~%")
              (when (fset:contains? new neggoal)
                (return-from goal-directed-infer-nlog
                             (values :c
                                (fset:convert 'list (fset:union new old))
                                step-no
                                (if track-sources
                                  (fset:convert 'hash-table sources)
                                  (make-hash-table :test #'equal)))))

              ; Increment step number and quit if it exceeds the limit (if any)
              (setf step-no (1+ step-no))
              (format t "Step number: ~s~%" step-no)
              (when max-steps
                (when (> step-no max-steps)
                  (format t "Max steps completed so return info.~%")
                  (return-from goal-directed-infer-nlog
                               (values :u
                                  (fset:convert 'list (fset:union new old))
                                  step-no
                                  (if track-sources
                                    (fset:convert 'hash-table sources)
                                    (make-hash-table :test #'equal))))))))))))

;; A global parameter of the current infer-nlog sources, a hash table.
;; This access is needed for the ULF marking dispatch functions.
(defparameter *infer-nlog-sources* (make-hash-table :test #'equal))
;; A dispatch function from inference rule functions to ULF marking functions.
;; Each inference rule may register a ULF marking function to circumvent the
;; slow generic ULF marking function.
(defparameter *infer-nlog-marking-dispatch-fn-ht* (make-hash-table :test #'equal))
(defparameter *default-marking-dispatch-timeout* 5)
(defun infer-nlog-source-mark-ulf-dispatch (ulf)
  "A dispatch function for ULF polarity marking based on the source rule.

  For the given ULF, this looks up which function and inputs led to this ULF
  and dispatches a marking function specific to that. The dispatch function
  will use the source ULFs and inference rule to generate a polarity marking
  for this ULF."
  (let* ((source (gethash ulf *infer-nlog-sources*))
         (source-fn (first source))
         (source-forms (second source))
         (mark-fn (gethash source-fn *infer-nlog-marking-dispatch-fn-ht*))
         dispatch-thread)

    ;; If no function, return nil.
    (when (not mark-fn)
      (return-from infer-nlog-source-mark-ulf-dispatch nil))

    ;; 1. Start the dispatch.
    (setf dispatch-thread
          (bt:make-thread
            (lambda ()
              (in-package :ulf-fracas)
              (handler-case
                (funcall mark-fn ulf source-forms)
                (error (e)
                  (format t "~%~%Error in dispatch thread from calling (~s ~s ~s), printing stack trace~%" mark-fn ulf source-forms)
                  (sb-debug:print-backtrace)
                  (format t "~%~%")
                  (error e))))))
    ;; 2. Join with timeout
    ;;    If timeout occurs, catch the error and return nil.
    ;;    Otherwise, return the result of the join
    (handler-case
      (bt:with-timeout (*default-marking-dispatch-timeout*)
        (bt:join-thread dispatch-thread))
      (bt:timeout (e)
        (bt:destroy-thread dispatch-thread)
        (format t "Dispatch function (~s) timed out, using default marker.~%" mark-fn)
        nil))))

;; Add the infer-nlog marking to the generic mark-ulf dispatch table and
;; define an infer-nlog specific mark-ulf function using this.
(setf (gethash 'infer-nlog *ulf-marking-dispatch-fn-ht*)
      #'infer-nlog-source-mark-ulf-dispatch)
(defun get-infer-nlog-marked-ulf (ulf)
  "Call the generic marking function with infer-nlog specific dispatch."
  (get-marked-ulf ulf 'infer-nlog))


;; A goal-directed A* search engine for monotonicity inferences on ULFs.
;; Search is done over a state space with states of the form (KB path-cost)
;; where the path-cost is the number of inferences required to reach the KB.
;; As input, takes a list of ULFs and a goal ULF, as well as, optionally, a cap
;; on the number of nodes to be explored and the heuristic function to use. If
;; no heuristic function is provided a constant heuristic is used, essentially
;; turning the search into a breadth-first search.
;; If the goal is found, returns a value list with:
;; - The goal ULF
;; - The path length to the goal
;; - The knowledge base in the goal state
;; - The set of KBs explored
;; If the goal is not found, returns a value list with NIL and the set of KBs
;; explored.
;; with real ULFs.
(defun infer-a* (in-ulfs goal &key max-iterations heuristic)
  (labels
    ; Default heuristic function for A* search
    ((default-heuristic (state goal-state)
        1)
     
     ; Get all children of a given search node. Search nodes look like '(KB path-cost)
     (get-a*-children (node)
       (let ((kb (first node))
             (cost (second node)))
         ; Build pairs of the form '(rule (args))
         (let ((unary-transitions (fset:convert 'list (build-unary-pairs kb)))
               (binary-transitions (fset:convert 'list (get-set-pairs
                                                         (fset:convert 'fset:set *binary-rules*)
                                                         (get-set-pairs kb kb)))))
           ; Apply rules and form a list of children, pruning out some duplicates
           (remove-if #'null
                      (mapcar
                        (lambda (x)
                          (let ((new (fset:convert 'fset:set x)))
                            (unless (fset:subset? new kb)
                              (list (fset:union kb new) (1+ cost)))))
                        (remove-if #'null
                                   (mapcar (lambda (x) (apply (first x) (second x)))
                                           (append unary-transitions
                                                   binary-transitions)))))))))
    ; Check if a heuristic is provided and fall back to default if not
    (unless heuristic
      (setf heuristic #'default-heuristic))
    ; Set initial values for search
    (let ((n 0)                                       ; Iteration number
          (node nil)                                  ; Current node
          (ulfs (fset:convert 'fset:set in-ulfs))     ; Initial KB
          (frontier (priority-queue:make-pqueue #'<)) ; Keys: path-cost + h(x)
                                                      ; Values: '(x path-cost)
          (explored (fset:empty-set)))                ; Explored set

      ; Push initial node to frontier
      (priority-queue:pqueue-push (list ulfs 0)
                                  (funcall heuristic ulfs goal)
                                  frontier)
      ; Main search loop
      (loop
        ; Increment iteration number and check if it exceeds bounds
        (setf n (1+ n))
        (when max-iterations
          (when (> n max-iterations)
            (return (values nil explored))))

        ; If at the end of the queue, fail
        (when (= (priority-queue:pqueue-length frontier) 0)
          (return nil))
        
        ; Pop node and check if it is a goal state
        (setf node (priority-queue:pqueue-pop frontier))
        (let ((kb (car node)))
          (if (fset:contains? kb goal)
            (return (values goal
                            (second node)
                            (first node)
                            explored
                            n)))
  
          ; Explore popped node
          (setf explored (fset:union explored (fset:set kb)))
          (let ((children (get-a*-children node)))
            (loop for child in children
                  do (if (not (fset:contains? explored (first child)))
                       (priority-queue:pqueue-push child
                                                   (+ (second child)
                                                      (funcall heuristic (first child) goal))
                                                   frontier)))))))))

;; Heuristic search with the same interface as the goal-directed code.
;; Returns
;;  - :e if entailed, :c if contradicted, and :u if not found
;;  - a list of all ulfs inferred,
;;  - the number of steps searched, and
;;  - a hash table of the inference sources (or nil if track-sources is nil).
(defun common-api-heuristic-search (inulfs ingoal &key max-iterations
                                           heuristic
                                           track-sources
                                           callpkg)
  (multiple-value-bind (code kb iter srcht)
                       (infer-heuristic-search inulfs ingoal
                                               :max-iterations max-iterations
                                               :heuristic heuristic
                                               :track-sources track-sources
                                               :callpkg callpkg)
    (let ((newcode (case code
                     (ENTAILMENT :e)
                     (CONTRADICTION :c)
                     (otherwise :u))))
      (values newcode kb iter srcht))))


;; External interface for infer-heuristic-search. See
;; internal-infer-heuristic-search for algorithm details.
(defun infer-heuristic-search (inulfs ingoal &key max-iterations
                                                  heuristic
                                                  track-sources
                                                  callpkg)
  (inout-intern (inulfs ulfs :ulf-fracas :callpkg callpkg)
    (in-intern (ingoal goal :ulf-fracas)
      (internal-infer-heuristic-search ulfs
                                       goal
                                       :max-iterations max-iterations
                                       :heuristic heuristic
                                       :track-sources track-sources))))

;; Heuristic-based search for goal-directed inference.
;;   PARAMETERS:
;;     * premises as a list of ULFs
;;     * goal ULF
;;     * (key param) max-iterations: a positive integer
;;     * (key param) heuristic: the heuristic function to use
;;   RETURNS: A value list with:
;;     * 'MAX-ITERS-REACHED, 'QUEUE-EMPTY, 'ENTAILMENT, or 'CONTRADICTION based
;;       on the result of the search
;;     * The knowledge base generated
;;     * The number of iterations executed
;;     * A hash table of the inference sources (or nil if track-sources is nil).
;; Heuristics should be functions as follows:
;;   INPUT: new-ulfs (fset), goal-ulf (S-expression), kb (fset)
;;   OUTPUT: a single integer. 0 if the goal was found, and -1 if a
;;   contradiction to the goal was found.
;; track-sources is a flag to keep track of sources of inferences in a hash
;; table, which is returned as a value. If track-sources is nil, an empty hash
;; table is returned.
(defun internal-infer-heuristic-search (ulfs goal &key max-iterations heuristic track-sources (queue-swap-width 5))
  (labels
    ; Default heuristic function
    ((default-heuristic (new-ulfs goal-ulf kb) 1))

    ;; START OF MAIN FUNCTION
    ; Check if a heuristic is provided and fall back to default if not
    (unless heuristic
      (setf heuristic #'default-heuristic))

    ;; Initial processing of ULFs.
    (multiple-value-bind (newulfs newgoal) (initial-ulf-processing ulfs goal)
      (setf ulfs newulfs)
      (setf goal newgoal))

    (let ((sources (if track-sources (fset:empty-map) nil)) 
          (n 0)
          (kb (fset:convert 'fset:set ulfs))
          (pq (priority-queue:make-pqueue #'<))
          (oq (priority-queue:make-pqueue #'<)) ; fifo ordered queue
          (unary-transitions nil)
          (binary-transitions nil)
          queue other-queue temp)

      ;; GK: The duplication of this with the loop-internal code really bugs
      ;; me. And in fact this led to a bug in my code merging because I missed
      ;; something in on the cases, not realizing it's duplicated.
      ; Build pairs of the form '(rule (args))
      (setf unary-transitions (fset:convert 'list (build-unary-pairs kb)))
      (setf binary-transitions (fset:convert 'list
                                             (get-set-pairs
                                               (fset:convert 'fset:set *binary-rules*)
                                               (get-set-pairs kb kb))))
      ; Apply rules and form a list of children, pruning out some duplicates
      (loop for (new srcinfo) in
            (remove-if (lambda (x) (or (fset:empty? (first x))
                                       (fset:subset? (first x) kb)))
                       (mapcar (lambda (x)
                                 (list (fset:convert
                                         'fset:set
                                         (mapcar
                                           #'postinf-normalize
                                           (apply (first x)
                                                  (second x))))
                                         x))
                               (append unary-transitions
                                       binary-transitions)))
           ; Add results to queues
           do 
           (progn
             ;; Store source info if track-sources is set.
             (when track-sources
               (fset:do-set (new-f new)
                            (setf sources (fset:with sources new-f srcinfo))
                            ;; Also add to global sources parameter for dispatch function access.
                            (setf (gethash new-f *infer-nlog-sources*) srcinfo)))
             (priority-queue:pqueue-push new
                                         (funcall heuristic new goal kb)
                                         pq)
             (priority-queue:pqueue-push new
                                         *step-cost*
                                         oq)))
      
      ; Main search loop
      (setf queue pq)
      (setf other-queue oq)
      (loop
        ; Swap queues.
        (when (= (mod n queue-swap-width) 0)
          (setf temp queue)
          (setf queue other-queue)
          (setf other-queue temp))

        ; If the maximum number of iterations has been reached, return failure
        (when max-iterations
          (when (> n max-iterations)
            (return (values 'MAX-ITERS-REACHED
                            kb
                            n
                            (fset:convert 'hash-table sources)))))

        ; If the priority queue is empty, return failure
        (when (priority-queue:pqueue-empty-p queue)
          (return (values 'QUEUE-EMPTY
                          kb
                          n
                          (fset:convert 'hash-table sources))))

        ; If the goal was found, return success
        (when (fset:contains? kb goal)
          (return (values 'ENTAILMENT
                          kb
                          n
                          (fset:convert 'hash-table sources))))

        ; Pop current ULFs and cost from the priority queue
        (multiple-value-bind (cur cost) (priority-queue:pqueue-pop queue)
          ; If the cost was 0, we found the goal
          (when (= cost 0)
            (return (values 'ENTAILMENT
                            (fset:union kb cur)
                            n
                            (fset:convert 'hash-table sources))))
          ; If the cost was -1, we found a contradiction
          (when (= cost -1)
            (return (values 'CONTRADICTION
                            (fset:union kb cur)
                            n
                            (fset:convert 'hash-table sources))))

          ; If this new value hasn't already been explored (e.g. via the other queue),
          ; then add and expand frontier.
          (when (not (fset:subset? cur kb))
            ; Find all new unary and binary transitions with current formulas
            (setf unary-transitions (fset:convert 'list (build-unary-pairs cur)))
            (setf binary-transitions
                  (append
                    (fset:convert
                      'list
                      (get-set-pairs (fset:convert 'fset:set *binary-rules*)
                                     (get-set-pairs cur kb)))
                    (fset:convert
                      'list
                      (get-set-pairs (fset:convert 'fset:set *binary-rules*)
                                     (get-set-pairs kb cur)))
                    (fset:convert
                      'list
                      (get-set-pairs (fset:convert 'fset:set *binary-rules*)
                                     (get-set-pairs cur cur)))))

            ; Apply them and add them to the queue
            (loop for (new srcinfo) in
                  (remove-if (lambda (x) (or (fset:empty? (first x))
                                             (fset:subset? (first x) kb)))
                             (mapcar (lambda (x)
                                       (list (fset:convert
                                               'fset:set
                                               (mapcar
                                                 #'postinf-normalize
                                                 (apply (first x)
                                                        (second x))))
                                             x))
                                     (append unary-transitions
                                             binary-transitions)))
                 do
                 (progn
                   ;; Store source info if track-sources is set.
                   (when track-sources
                     (fset:do-set (new-f new)
                       (setf sources (fset:with sources new-f srcinfo))
                       ;; Also add to global sources parameter for dispatch function access.
                       (setf (gethash new-f *infer-nlog-sources*) srcinfo)))
                   ;; Add to heuristic queue
                   (priority-queue:pqueue-push new
                                               (+ (funcall heuristic new goal kb)
                                                  (* *step-cost* n)) ; Added weight to avoid going into an infinite depth-first dive
                                               pq)
                   ;; Add to BFS queue
                   (priority-queue:pqueue-push new
                                               (* *step-cost* (1+ n))
                                               oq)))

            ; Update loop variables
            (setf kb (fset:union kb cur))
            (setf n (1+ n))))))))

