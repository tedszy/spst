(in-package #:spst)

(defmacro make-barning (a b c d e f g h i)
  `(make-array 9
	       :element-type 'integer
	       :initial-contents '(,a ,b ,c ,d ,e ,f ,g ,h ,i)))

(defparameter *barning-a* (make-barning 1 -2 2 2 -1 2 2 -2 3))
(defparameter *barning-b* (make-barning 1 2 2 2 1 2 2 2 3))
(defparameter *barning-c* (make-barning -1 2 2 -2 1 2 -2 2 3))

;; Paths are lists of what matrices you must apply to get to (a,b,c),
;; in reverse order (because cons builds them that way.) Path 
;; information is stored in the path slot whenever we obtain a ppt
;; by Barning matrix multiplication.
(defclass primitive-pythagorean-triple ()
  ((a :accessor pp.a :initarg :a)
   (b :accessor pp.b :initarg :b)
   (c :accessor pp.c :initarg :c)
   (path :accessor pp.path :initarg :path :initform nil)))

(defun ppt (a b c &optional path) 
  (make-instance 'primitive-pythagorean-triple :a a :b b :c c :path path))

(defmethod print-object ((ppt primitive-pythagorean-triple) stream)
  (with-slots (a b c path) ppt
    (format stream "(~s, ~s, ~s, ~s)" a b c path)))

(defun ppt-equal (p q)
  (with-slots (a b c) p
    (with-slots ((a1 a) (b1 b) (c1 c)) q
      (and (= a a1) (= b b1) (= c c1)))))

;; Multiplication by one set of Barning matricies. Another set
;; was discovered not long ago. Of course many optimizations
;; are possible.
(defun mul-a (ppt)
  (with-slots (a b c) ppt
    (ppt (+ (* 1 a) (* -2 b) (* 2 c))
	 (+ (* 2 a) (* -1 b) (* 2 c))
	 (+ (* 2 a) (* -2 b) (* 3 c))
	 (cons 'A (pp.path ppt)))))

(defun mul-b (ppt)
  (with-slots (a b c) ppt
    (ppt (+ (* 1 a) (* 2 b) (* 2 c))
	 (+ (* 2 a) (* 1 b) (* 2 c))
	 (+ (* 2 a) (* 2 b) (* 3 c))
	 (cons 'B (pp.path ppt)))))

(defun mul-c (ppt)
  (with-slots (a b c) ppt
    (ppt (+ (* -1 a) (* 2 b) (* 2 c))
	 (+ (* -2 a) (* 1 b) (* 2 c))
	 (+ (* -2 a) (* 2 b) (* 3 c))
	 (cons 'C (pp.path ppt)))))

(defgeneric valid-p (object)
  (:documentation "Is object a valid tuple?"))

;; Pairwise coprime. Middle term is even. Satisfy a^2 + b^2 = c^2.
(defmethod valid-p ((ppt primitive-pythagorean-triple))
  (with-slots (a b c) ppt
    (and (evenp b)
	 (= (* c c) 
	    (+ (* a a) (* b b)))
	 (= (gcd a b) (gcd b c) (gcd c a) 1))))

;; This may be a very good demonstration of why lists 
;; are sometimes incredibly efficient. The data in the 
;; path slots is not duplicated. 
(defun depth-search-ppt ()
  (let ((stack (list (ppt 3 4 5)))
	(count 0)
	(result '())
	(limit 10000))
    (loop 
       while (not (null stack))
       do (let* ((ppt (pop stack))
		 (ppta (mul-a ppt))
		 (pptb (mul-b ppt))
		 (pptc (mul-c ppt)))
	    (incf count)
	    (when (with-slots (a b c) ppt (= 0 (mod (+ a b c) 91)))
	      (push ppt result))
	    (when (<= (with-slots (a b c) ppta (+ a b c)) limit)
	      (push ppta stack))
	    (when (<= (with-slots (a b c) pptb (+ a b c)) limit)
	      (push pptb stack))
	    (when (<= (with-slots (a b c) pptc (+ a b c)) limit)
	      (push pptc stack)))
       finally (return (list count result)))))

(defun follow-path (path)
  (reduce #'(lambda (p m)
	      (case m
		(A (mul-a p))
		(B (mul-b p))
		(C (mul-c p))))
	  path
	  :initial-value (ppt 3 4 5)))









;; Tests.

(setq *print-failures* t)

(define-test barning-matricies
  (let ((p (ppt 3 4 5)))
    (assert-true (ppt-equal (ppt 5 12 13) (mul-a p)))
    (assert-true (ppt-equal (ppt 21 20 29) (mul-b p)))
    (assert-true (ppt-equal (ppt 15 8 17) (mul-c p)))))

(define-test validity-checking
  (assert-true (valid-p (ppt 3 4 5)))
  (assert-true (valid-p (ppt 115 252 277)))
  (assert-false (valid-p (ppt 21 20 39)))
  (assert-false (valid-p (ppt 4 3 5))))

(define-test following-paths
  (let ((pth '(A A A A B B B A B C A B C)))
    (assert-true (ppt-equal (ppt 10764945 55503968 56538257)
			    (follow-path (reverse pth))))))


