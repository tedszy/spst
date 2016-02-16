(in-package #:spst)

;; Continued fractions.

(defun rational-to-contfrac (rat)
  (loop
     with m = (numerator rat)
     with d = (denominator rat)
     with cf = '()
     until (= d 0)
     do (multiple-value-bind (q r)
	    (floor m d)
	  (push q cf)
	  (setf m d)
	  (setf d r))
     finally (return (reverse cf))))

(defun convergents (cf)
  (let ((a0 (first cf))
	(a1 (second cf)))
    (loop 
       with ps = (list (+ (* a0 a1) 1) a0)
       with qs = (list a1 1)    
       for a in (cddr cf)
       do (progn 
	    (push (+ (* (first ps)
			a)
		     (second ps))
		  ps)
	    (push (+ (* (first qs)
			a)
		     (second qs))
		  qs))
       finally (return (reverse (mapcar #'/ ps qs))))))

;; Tenner algorithm for continued fraction expansion of sqrt(d).
(defun tenner (d num-terms)
  (let* ((k (isqrt d))
	 (r (- d (* k k)))
	 (a k) (b 0) (c k)
	 (u d) (v 0) (w r)
	 (result nil))
    (loop 
       repeat num-terms
       do 
	 (push a result)
	 (multiple-value-bind (qq rr)
	     (floor (+ k c) w)
	   (setf a qq
		 b rr
		 c (- k b)
		 u (* c c)
		 v (- d u)
		 w (/ v w)))
       finally (return (reverse result)))))
	 



(define-test continued-fractions
  (assert-equalp (rational-to-contfrac 33/137) '(0 4 6 1 1 2))
  (assert-equalp (rational-to-contfrac 137/33) '(4 6 1 1 2))
  (assert-equalp (convergents (rational-to-contfrac 137/33))
		 '(4 25/6 29/7 54/13 137/33))
  (assert-equalp (convergents (rational-to-contfrac 33/137))
		 '(0 1/4 6/25 7/29 13/54 33/137))
  (assert-equalp (tenner 29 7) '(5 2 1 1 2 10 2))
  )
