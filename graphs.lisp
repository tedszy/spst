;;(in-package :spst)

;;
;; Graphs
;;




;; Graph base class.
(defclass graph ()
  ((nodes :accessor graph.nodes :initarg :nodes :initform nil)
   (edges :accessor graph.edges :initarg :edges :initform nil)
   (num-nodes :accessor graph.num-nodes :initarg :num-nodes)
   (num-edges :accessor graph.num-edges :initarg :num-edges)))

(defclass graph-with-weighted-edges (graph) ())

(defclass graph-with-weighted-nodes (graph) ())

(defclass graph-with-weighted-edges-and-nodes (graph) ())
  


(defmethod print-object ((g graph) stream)
  (with-slots (nodes edges num-nodes num-edges) g
    (format stream 
	    "[nodes:~a edges:~a]"
	    (graph.num-nodes g)
	    (graph.num-edges g))))

(defun make-graph-with-weighted-nodes (node-list edge-list)
  (make-instance 'graph-with-weighted-nodes
		 :nodes node-list
		 :edges edge-list
		 :num-nodes (length node-list)
		 :num-edges (length edge-list)))




(defgeneric bellman-ford (g source))

(defmethod bellman-ford ((g graph-with-weighted-nodes) source)
  (let ((distances (make-hash-table))
	(predecessors (make-hash-table))
	(node-weights (make-hash-table)))
     ;; initialize node-weight, distance and predecessor tables.
    (loop 
       for node in (graph.nodes g)
       do
	 (setf (gethash (first node) node-weights) (second node))
	 (setf (gethash (first node) distances) nil)
	 (setf (gethash (first node) predecessors) nil))
    ;; source node is special case.
    (setf (gethash source distances) 0)
    (setf (gethash source predecessors) nil)
    ;; relaxation loop
    (loop
       for k from 1 to (graph.num-nodes g)
       do
	 (loop 
	    for (u v) in (graph.edges g)
	    do
	      (let ((u.d (gethash u distances))
		    (v.d (gethash v distances))
		    (u.w (gethash u node-weights)))
		(when (and u.d v.d (< (+ u.d u.w) v.d))
		  (setf (gethash v distances) (+ u.d u.w))
		  (setf (gethash v predecessors) u))
		(when (and u.d (null v.d))
		  (setf (gethash v distances) (+ u.d u.w))
		  (setf (gethash v predecessors) u)))))
    (loop 
       for n in (graph.nodes g)
       as id = (first n)
       with bellman-table = (make-hash-table)
       do
	 (setf (gethash id bellman-table) 
	       (list id
		     (gethash id distances)
		     (gethash id predecessors)
		     (gethash id node-weights)))
       finally (return bellman-table))))


(defun optimal-path (target bellman-table)
  (loop 
     with path = (list target)
     with current = target
     as predecessor = (third (gethash current bellman-table))
     while predecessor
     do
       (push predecessor path)
       (setf current predecessor)
     finally (return path)))

;; Since the nodes are weighted, and the distance is
;; the sum of the previous nodes, we've got to add the
;; cost of the final target node.
(defun solution (g source target)
  (let* ((bellman-table (bellman-ford g source))
	 (tt (gethash target bellman-table)))
    (values (+ (second tt)
	       (fourth tt))
	    (optimal-path target bellman-table))))






















;;
;;
;; Test of weighted nodes.
;;
;;

(defparameter data-1
  (make-array '(5 5)
	      :initial-contents '((131 673 234 103 18)
				  (201 96  342 965 150)
				  (630 803 746 422 111)
				  (537 699 487 121 956)
				  (805 732 534 37  331))))


;; node-list, in order. Weighted nodes.
(defparameter node-list-1
  (loop 
     for i from 0 below (array-dimension data-1 0)
     with n = 0
     with nodes = nil
     do (loop
	   for j from 0 below (array-dimension data-1 1)
	   do 
	     (push (list n (aref data-1 i j)) nodes)
	     (incf n))
     finally (return (reverse nodes))))
       
(defparameter edge-list-1
  (let ((nrows (array-dimension data-1 0))
	(ncols (array-dimension data-1 1))
	(edges nil))
    (print nrows)
    (print ncols)
    (loop 
       for i from 0 below nrows
       do
	 (loop
	    for j from 0 below ncols 
	    if (> j 0) 
	    do (push (list (+ (* ncols i) j) 
			   (+ (* ncols i) (1- j))) edges) ;; left
	    if (< j (1- ncols))
	    do (push (list (+ (* ncols i) j) 
			   (+ (* ncols i) (1+ j))) edges) ;; right
	    if  (> i 0) 
	    do (push (list (+ (* ncols i) j) 
			   (+ (* ncols (1- i)) j)) edges) ;; up
	    if (< i (1- nrows))
	    do (push (list (+ (* ncols i) j)
			   (+ (* ncols (1+ i)) j)) edges))) ;; down
    edges))

(defparameter g-1 
  (make-graph-with-weighted-nodes node-list-1 edge-list-1))








;; (0 5 6 7 2 3 4 9 14 13 18 23 24)



















#|

;; Graphs defined by an edge lists with weights.
;; The other slots in the graph object follow.
;; All paths to be analyzed start at the source node.
(defclass graph ()
  ((nodes :accessor graph.nodes :initarg :nodes :initform nil)
   (edges :accessor graph.edges :initarg :edges :initform nil)
   (num-nodes :accessor graph.num-nodes :initarg :num-nodes)
   (num-edges :accessor graph.num-edges :initarg :num-edges)
   (source :accessor graph.source :initarg :source)))



;; Nodes are weighted, edges are not.
(defclass weighted-node-graph (graph)
  ())




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
    (let ((new-node-list (cons source (sort node-list ordering))))
      (make-instance 'graph
		     :nodes new-node-list
		     :edges edge-list
		     :num-nodes (length new-node-list)
		     :num-edges (length edge-list)
		     :source source))))

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
	(predecessors (make-hash-table))
	(d-table nil)   ;; distance table (as done on whiteboard)
	(pi-table nil)) ;; precedence table (as done on whiteboard)
    ;; Initialize distance table and predecessor table.
    (setf (gethash (graph.source g) distances) 0)
    (setf (gethash (graph.source g) predecessors) nil)
    (loop for v in (cdr (graph.nodes g))
       do 
	 (setf (gethash v distances) nil)
	 (setf (gethash v distances) nil))  ;; why twice?
    ; Main relaxation loop.
    (loop 
       ;; temporary hack -- fix the stopping condition.
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
	 ;; if show-steps is true, collect the steps.
	 (when show-steps
	   (push (loop for v in (graph.nodes g)
		    collecting (gethash v distances)) d-table)
	   (push (loop for v in (graph.nodes g)
		    collecting (gethash v predecessors)) pi-table)))

    ;; If show-steps is true, print the so-called d and pi tables.
    (when show-steps
      (format t "distance table:~%")
      (format t "~{~5<~a~>~^ ~}~%" (graph.nodes g))
      (loop for row in (reverse d-table)
	   do (format t "~{~5<~a~>~^ ~}~%" row))
      (format t "~%")
      (format t "predecessor table:~%")
      (format t "~{~5<~a~>~^ ~}~%" (graph.nodes g))
      (loop for row in (reverse pi-table)
	   do (format t "~{~5<~a~>~^ ~}~%" row)))

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


|#
