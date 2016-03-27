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

(defun make-graph-with-weighted-edges (node-list edge-list)
  (make-instance 'graph-with-weighted-edges
		 :nodes node-list
		 :edges edge-list
		 :num-nodes (length node-list)
		 :num-edges (length edge-list)))

(defgeneric bellman-ford (g source))

(defmethod bellman-ford ((g graph-with-weighted-edges) source)
  (let ((distances (make-hash-table))
	(predecessors (make-hash-table)))
     ;; initialize node-weight, distance and predecessor tables.
    (loop 
       for id in (graph.nodes g)
       do
	 (setf (gethash id distances) nil)
	 (setf (gethash id predecessors) nil))
    ;; source node is special case.
    (setf (gethash source distances) 0)
    (setf (gethash source predecessors) nil)
    ;; relaxation loop
    (loop
       for k from 1 to (graph.num-nodes g)
       do (loop 
	     for (u v w) in (graph.edges g)
	     do (let ((u.d (gethash u distances))
		      (v.d (gethash v distances)))
		  (when (or (and u.d v.d (< (+ u.d w) v.d))
			    (and u.d (null v.d)))
		    (setf (gethash v distances) (+ u.d w))
		    (setf (gethash v predecessors) u)))))
    ;; Bellman results data is a table of distances (costs) 
    ;; and predecessors. 
    (loop 
       for id in (graph.nodes g)
       with bellman-table = (make-hash-table)
       do (setf (gethash id bellman-table) 
		(list id
		      (gethash id distances)
		      (gethash id predecessors)))
       finally (return bellman-table))))

(defmethod bellman-ford ((g graph-with-weighted-nodes) source)
  (let ((distances (make-hash-table))
	(predecessors (make-hash-table))
	(node-weights (make-hash-table)))
     ;; initialize node-weight, distance and predecessor tables.
    (loop 
       for node in (graph.nodes g)
       do (setf (gethash (first node) node-weights) (second node))
	 (setf (gethash (first node) distances) nil)
	 (setf (gethash (first node) predecessors) nil))
    ;; source node is special case.
    (setf (gethash source distances) 0)
    (setf (gethash source predecessors) nil)
    ;; relaxation loop
    (loop
       for k from 1 to (graph.num-nodes g)
       do (loop 
	     for (u v) in (graph.edges g)
	     do (let ((u.d (gethash u distances))
		      (v.d (gethash v distances))
		      (u.w (gethash u node-weights)))
		  (when (or (and u.d v.d (< (+ u.d u.w) v.d))
			    (and u.d (null v.d)))
		    (setf (gethash v distances) (+ u.d u.w))
		    (setf (gethash v predecessors) u)))))
    ;; Bellman results data is a table of distances (costs) 
    ;; and predecessors. 
    (loop 
       for n in (graph.nodes g)
       as id = (first n)
       with bellman-table = (make-hash-table)
       do (setf (gethash id bellman-table) 
		(list id
		      (gethash id distances)
		      (gethash id predecessors)))
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

(defgeneric optimal-solution (graph source target))

(defmethod optimal-solution ((g graph-with-weighted-nodes) source target)
;; Since the nodes are weighted, and the distance is
;; the sum of the previous nodes, we've got to add the
;; cost of the final (target) node.
  (let* ((bellman-table (bellman-ford g source))
	 (tt (gethash target bellman-table))
	 (target-node-weight (second (find target (graph.nodes g) :key #'first))))
    (values (+ (second tt)
	       target-node-weight)
	    (optimal-path target bellman-table))))

(defmethod optimal-solution ((g graph-with-weighted-edges) source target)
  (let* ((bellman-table (bellman-ford g source))
	 (tt (gethash target bellman-table)))
    (values (second tt)
	    (optimal-path target bellman-table))))

;;
;;
;; Test of graph with weighted nodes.
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

;; 2297
;; (0 5 6 7 2 3 4 9 14 13 18 23 24)

;;
;;
;; Test graph with weighted edges and symbols for nodes.
;;
;;

(defparameter node-list-2 '(s a b c d)) 

(defparameter edge-list-2
  '((a c 3) (b a -5) (b d 1) (c d 2) (s a 4) (s b 6)))

(defparameter g-2
  (make-graph-with-weighted-edges node-list-2 edge-list-2))

;; CL-USER> (solution g-2 'S 'D)
;; 6
;; (S B A C D)

;;
;;
;; Another graph with weighted edges.
;;
;;

(defparameter data-3
  '((131 673 234 103 18)
    (201 96  342 965 150)
    (630 803 746 422 111)
    (537 699 487 121 956)
    (805 732 534 37  331)))

(defparameter node-list-3
  (loop for n from 0 to 25 collecting n))

;; Build an edge list from the raw data,
;; interpreting the numbers as edge weights.
(defparameter edge-list-3 
  (let* ((nrows (length data-3))
	 (ncols (length (car data-3)))
	 (edges nil)
	 ;; Rows of nodes, not including the source node 0.
	 (nodes (loop 
		   for k from 1 to nrows 
		   collecting (loop 
				 for j from 1 to ncols
				 collecting (+ (* (1- k) ncols) j)))))

   ;; Build edges going to the right.
    (loop 
       for rown in nodes
       and roww in data-3
       do 
	 (loop 
	    for (u v) on rown
	    and w in (cdr roww)
	    while v
	    do (push (list u v w) edges)))

    ;; Build edges going down.
    (loop 
       for (rown rown-next) on nodes
       and roww in (cdr data-3)
       while rown-next 
       do
	 (loop
	    for u in rown
	    and v in rown-next
	    and w in roww
	    do (push (list u v w) edges)))

    ;; Now push back the 0 node with a wieght of data[0,0].
    (push (list 0 1 (caar data-3)) edges)))

(defparameter g-3
  (make-graph-with-weighted-edges node-list-3 edge-list-3))

;; CL-USER> (solution g-3 0 25)
;; 2427
;; (0 1 6 7 8 13 14 19 24 25)



