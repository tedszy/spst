(in-package #:spst)

;; If you don't need Pythagorean tree path info, use this.
(defclass pythagorean-tuple ()
  ((a :accessor pytuple.a :initarg :a)
   (b :accessor pytuple.b :initarg :b)
   (c :accessor pytuple.c :initarg :c)))

;; Paths are stored in reverse because cons builds them that way.
(defclass pythagorean-tuple-with-path (pythagorean-tuple)
  ((revpath :accessor pytuple.revpath :initarg :revpath :initform nil)))

(defun make-pytuple (a b c) 
  (make-instance 'pythagorean-tuple :a a :b b :c c))

(defun make-pytuple-with-path (a b c &optional revpath)
  (make-instance 'pythagorean-tuple-with-path 
		 :a a :b b :c c :revpath revpath)) 

(defmethod print-object ((pt pythagorean-tuple) stream)
  (with-slots (a b c) pt
    (format stream "(~s, ~s, ~s)" a b c)))

;; Print out the reversed revpath, i.e, the path.
(defmethod print-object ((pt pythagorean-tuple-with-path) stream)
  (with-slots (a b c revpath) pt
    (format stream "(~s, ~s, ~s => ~s)" a b c (reverse revpath))))
	    
(defgeneric tuple= (tup1 tup2)
  (:documentation "Are two tuples equal?"))

(defmethod tuple= ((tup1 pythagorean-tuple) (tup2 pythagorean-tuple))
  (with-slots (a b c) tup1
    (with-slots ((a1 a) (b1 b) (c1 c)) tup2
      (and (= a a1) (= b b1) (= c c1)))))

;; Make this a generic function because we may need other methods
;; of it for other tuples: for example the two sorts of coprime tuples.
(defgeneric valid-p (object)
  (:documentation "Is object a valid tuple?"))

;; Valid primitive Pythagorean tuple (a,b,c).
;; Pairwise coprime. Middle term is even. Satisfies a^2 + b^2 = c^2.
;; c is bigger than a and b. a and c are odd.
(defmethod valid-p ((pt pythagorean-tuple))
  (with-slots (a b c) pt
    (and (evenp b)
	 (and (oddp a) (oddp c))
	 (and (> c a) (> c b))
	 (= (* c c) 
	    (+ (* a a) (* b b)))
	 (= (gcd a b) (gcd b c) (gcd c a) 1))))

;; Navigating the Tree without constructing paths. Of course
;; this is faster and uses less memory than if we were building paths.
(defgeneric barning-mul (branch pt)
  (:documentation "Get the branch (child) of the current tuple
by matrix multiplication."))

;; Multiplication by one set of Barning matrices. Another set
;; was discovered not long ago. Many optimizations are possible. 
;; Dispatch on symbols 'A, 'B, 'C.
(defmethod barning-mul ((branch symbol) (pt pythagorean-tuple))
  (with-slots (a b c) pt
    (case branch
      (A (make-pytuple (+ (* 1 a) (* -2 b) (* 2 c))
		       (+ (* 2 a) (* -1 b) (* 2 c))
		       (+ (* 2 a) (* -2 b) (* 3 c))))
      (B (make-pytuple (+ (* 1 a) (* 2 b) (* 2 c))
		       (+ (* 2 a) (* 1 b) (* 2 c))
		       (+ (* 2 a) (* 2 b) (* 3 c))))
      (C (make-pytuple (+ (* -1 a) (* 2 b) (* 2 c))
		       (+ (* -2 a) (* 1 b) (* 2 c))
		       (+ (* -2 a) (* 2 b) (* 3 c)))))))

;; Same kind of branching multiplication, but with path information recorded.
(defmethod barning-mul ((branch symbol) (pt pythagorean-tuple-with-path))
  (with-slots (a b c revpath) pt
    (case branch
      (A (make-pytuple-with-path
	  (+ (* 1 a) (* -2 b) (* 2 c))
	  (+ (* 2 a) (* -1 b) (* 2 c))
	  (+ (* 2 a) (* -2 b) (* 3 c))
	  (cons 'A revpath)))
      (B (make-pytuple-with-path
	  (+ (* 1 a) (* 2 b) (* 2 c))
	  (+ (* 2 a) (* 1 b) (* 2 c))
	  (+ (* 2 a) (* 2 b) (* 3 c))
	  (cons 'B revpath)))
      (C (make-pytuple-with-path 
	  (+ (* -1 a) (* 2 b) (* 2 c))
	  (+ (* -2 a) (* 1 b) (* 2 c))
	  (+ (* -2 a) (* 2 b) (* 3 c))
	  (cons 'C revpath))))))

;; Find the pythagorean tuple corresponding to the given path,
;; which is a list of symbols 'A, 'B, 'C.
(defun follow-path (path)
  (reduce #'(lambda (pt branch)
	      (barning-mul branch pt))
	  path
	  :initial-value (make-pytuple-with-path 3 4 5)))

;; This may be a very good demonstration of why lists 
;; are sometimes incredibly efficient. The data in the 
;; path slots is not duplicated. 

;; If :with-path is set to true, the computation uses 
;; pythagorean-tuple-with-path objects, and their paths 
;; are recorded because the methods of the generic functions 
;; know how to handle them.
(defun depth-search-pytree (&key search-for limit (with-path nil))
  (let ((stack (list (if with-path
			 (make-pytuple-with-path 3 4 5)
			 (make-pytuple 3 4 5))))
	(count 0)
	(result '()))
    (loop 
       while (not (null stack))
       do (let* ((pt (pop stack))
		 (pta (barning-mul 'A pt))
		 (ptb (barning-mul 'B pt))
		 (ptc (barning-mul 'C pt)))
	    (incf count)
	    (when (funcall search-for pt) (push pt result))
	    (when (funcall limit pta) (push pta stack))
	    (when (funcall limit ptb) (push ptb stack))
	    (when (funcall limit ptc) (push ptc stack)))
       finally (return (values result 
			       count)))))

;; Try it out. (115,252,277) is a primitive pythagorean triple.
;; Are there triples elementwise divisible by this triple?
;; If you pass it :with-path t, it will also calculate paths
;; through the pythagorean tree.
(defun try-it (&key (with-path nil))
  (depth-search-pytree 
   :search-for #'(lambda (pt) 
		   (with-slots (a b c) pt
		     (and (= 0 (mod a 115))
			  (= 0 (mod b 252))
			  (= 0 (mod c 277)))))
   :limit #'(lambda (pt) 
	      (with-slots (a b c) pt
		(<= c 10000000)))
   :with-path with-path))


;; Tests.

(setq *print-failures* t)

(define-test barning-matricies
  (let ((pt (make-pytuple 3 4 5)))
    (assert-true (tuple= (make-pytuple 5 12 13) (barning-mul 'A pt)))
    (assert-true (tuple= (make-pytuple 21 20 29) (barning-mul 'B pt)))
    (assert-true (tuple= (make-pytuple 15 8 17) (barning-mul 'C pt)))))

(define-test validity-checking
  (assert-true (valid-p (make-pytuple 3 4 5)))
  (assert-true (valid-p (make-pytuple 115 252 277)))
  (assert-false (valid-p (make-pytuple 21 20 39)))
  (assert-false (valid-p (make-pytuple 4 3 5))))

(define-test following-paths
  (let ((revpath '(A A A A B B B A B C A B C)))
    (assert-true (tuple= (make-pytuple 10764945 55503968 56538257)
			 (follow-path (reverse revpath))))))


