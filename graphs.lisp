(in-package :spst)

;;
;; Graphs
;;

;; Graphs defined by an edge lists with weights.
;; The other slots in the graph object follow.
;; All paths to be analyzed start at the source node.
(defclass graph ()
  ((nodes :accessor graph.nodes :initarg :nodes :initform nil)
   (edges :accessor graph.edges :initarg :edges :initform nil)
   (num-nodes :accessor graph.num-nodes :initarg :num-nodes)
   (num-edges :accessor graph.num-edges :initarg :num-edges)
   (source :accessor graph.source :initarg :source)))

;; If your node labels are symbols, pass this into make-graph.
;; If plain integers, use #'<.
(defun symbol-test (x y) (string< (symbol-name x) (symbol-name y)))
    
;; remove source node, sort the rest, cons back the source node.
;; Ordering funtion determines how you'd like your nodes
;; ordered, after the source node, which is always first.
(defun make-graph (edge-list &key source (ordering #'<))
  (let ((node-list (remove source 
			   (loop 
			      for e in edge-list
			      with result = nil
			      do 
				(pushnew (first e) result) 
				(pushnew (second e) result)
			      finally (return result)))))
    (make-instance 'graph
		   :nodes (cons source (sort node-list ordering))
		   :edges edge-list
		   :num-nodes (length node-list)
		   :num-edges (length edge-list)
		   :source source)))

(defmethod print-object ((g graph) stream)
  (with-slots (nodes edges num-nodes num-edges) g
    (format stream 
	    "[nodes:~a edges:~a ~a]"
	    (graph.num-nodes g)
	    (graph.num-edges g)
	    (graph.edges g))))

;;
;;
;; Bellman-Ford solver.
;;
;;

;; You can look for biggest paths or smallest 
;; paths. Adjust minmax argument accordingly.
(defun bellman-ford (g &key (show-steps nil) (minmax #'<)) 
  (let ((distances (make-hash-table))
	(predecessors (make-hash-table)))    
    ;; Initialize distance table and predecessor table.
    (setf (gethash (graph.source g) distances) 0)
    (setf (gethash (graph.source g) predecessors) nil)
    (loop for v in (cdr (graph.nodes g))
       do 
	 (setf (gethash v distances) nil)
	 (setf (gethash v distances) nil))
    ; Main relaxation loop.
    (loop 
       for k from 1 to (graph.num-nodes g)
       do
	 ; One iteration of Bellman Ford starts here.
	 (loop 
	    for (u v weight) in (graph.edges g)
	    do (let ((u.d (gethash u distances))
		     (v.d (gethash v distances)))
		 (when (or (and u.d (eql v.d nil))
			   (and u.d 
				v.d 
				(funcall minmax (+ u.d weight) v.d)))
		   (setf (gethash v distances) (+ u.d weight))
		   (setf (gethash v predecessors) u))))
	 ;; if show-steps is true...
	 (when show-steps
	   (format t 
		   "~a~%" 
		   (loop 
		      for v in (graph.nodes g) 
		      collecting (gethash v distances)))
	   (format t 
		   "~a~%" 
		   (loop 
		      for v in (graph.nodes g) 
		      collecting (gethash v predecessors)))))
    (values distances predecessors)))
  
;; Constructs optimal path and path cost from 
;; Bellman-Ford return values.
(defun optimal-path (node distances predecessors)
  (let ((cost (gethash node distances)))
    (loop
       with path = (list node)
       with current = node
       as pred = (gethash current predecessors)
       while pred
       do
	 (push pred path)
	 (setf current pred)
	 finally (return (values cost path)))))

;;
;;
;; Test a small graph.
;;
;;

(defparameter edge-list-1
  '((a c 3) (b a -5) (b d 1) (c d 2) (s a 4) (s b 6)))

(defparameter g1 (make-graph edge-list-1 :source 'S :ordering #'symbol-test))

(defun test-me ()
  (multiple-value-bind (d p) (bellman-ford g1)
    (optimal-path 'd d p)))
