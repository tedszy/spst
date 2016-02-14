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



;; Strings.

(defun split-comma-string (str)
  (loop 
     with foo = (lambda (c) (char= c #\,))
     for a = (position-if-not foo str)
     then (position-if-not foo str :start (1+ b))
     for b = (and a 
		  (position-if foo str :start a))
     when a collect (subseq str a b)
     while b))

(defun join (out list-of-strings delimiter)
  "If you use t for the output stream, it goes to standard output. 
   And you can use \"~%\" for the delimiter to get items separated 
   by newlines."
  (let ((fmt (format nil "~a~a~a" "~{~a~^" delimiter "~}")))
    (format out fmt list-of-strings)))

(defun join-newline (out list-of-strings)
  "Convenience functions. Joins a list of strings with a newline separator."
  (join out list-of-strings "~%"))





(defun make-prime-sieve (size)
  (let ((sieve (make-array size 
			   :element-type 'bit 
			   :initial-element 1)))
    (setf (bit sieve 0) 0)
    (setf (bit sieve 1) 0)
    (loop 
       for i from 0 below size
       when (= 1 (bit sieve i))
       do (loop
	     for k from 2
	       until (> (* k i) (1- size))
	       do (setf (bit sieve (* k i)) 0)))
    sieve))

(defun make-distinct-prime-factor-count-sieve (sieve)
  (let* ((size (length sieve))
	 (result (make-array size 
			     :element-type 'fixnum
			     :initial-element 0)))
    (loop 
       for i from 0 below size
       when (= 1 (bit sieve i))
       do (loop
	     for k from 1
	     for idx = (* k i)
	     until (> idx (1- size))
	     do (incf (aref result idx))))
    result))

(defun make-totient-sieve (sieve)
  (let* ((size (length sieve))
	 (result (make-array size
			     :element-type 'fixnum
			     :initial-element 1)))
    (progn (loop for k from 0 below size
	      do (setf (aref result k)
		       (setf (aref result k) k)))
	   (loop
	      for i from 0 below size
	      when (= 1 (bit sieve i))
	      do (loop
		    for k from 1
		    for idx = (* k i)
		    until (> idx (1- size))
		    do (setf (aref result idx)
			     (/ (* (aref result idx)
				   (1- i))
				i))))
	   result)))
	 
(defun make-prime-predicate (sieve)
  (lambda (n)
    (= 1 (aref sieve n))))

(defun binomial (n k)
  (cond ((< n k) 0)
	((= n k) 1)
;;	((> k (- n k))   ;; what's the problem here?
;;	 (setf k (- n k)))
	(t 
	 (let ((result 1))
	   (loop
	      for i from 1 upto k
	      do (setf result (* result n))
		(setf result (/ result i))
		(decf n))
	   result))))

(defun number-of-digits (n &key (base 10))
  (if (= n 0)
      1
      (loop until (= n 0)
	 do (setf n (floor n base))
	 counting 1)))

(defun integer-to-digit-list (n &key (base 10))
  (if (zerop n)
      '(0)
      (loop with result = '() 
	 while (plusp n)
	 do (multiple-value-bind (q r)
		(floor n base)
	      (push r result)
	      (setf n q))
	 finally (return result))))

(defun integer-to-digit-vector (n &key (base 10))
  (if (zerop n)
      #(0)
      (let ((ndigits (number-of-digits n :base base)))
	(loop with result = (make-array ndigits 
					:element-type 'fixnum)
	   with j = (- ndigits 1)
	   while (plusp n)
	   do (multiple-value-bind (q r)
		  (floor n base)
		(setf (aref result j) r)
		(setf n q)
		(decf j))
	   finally (return result)))))

(defun digit-sequence-to-integer (dv &key (base 10))
  (reduce #'(lambda (acc u)
	      (+ u (* base acc))) dv :initial-value 0))

(defun swapf (vec i j)
  (let ((temp (aref vec i)))
    (setf (aref vec i) (aref vec j))
    (setf (aref vec j) temp)))

(defun reversef (vec start)
  (setf (subseq vec start)  
	(reverse (subseq vec start))))

;; Before you use this, make sure vec is not 
;; at the last lexical permutation. 
(defun lexically-permutef (vec)
  (let ((vlen (length vec)))
    (multiple-value-bind (suffix pivot)
	(loop 
	   with suffix = (- vlen 1)
	   with pivot = (- vlen 2)
	   until (or (= 0 suffix)
		     (< (aref vec pivot)
			(aref vec suffix)))
	   do (progn (decf suffix)
		     (decf pivot))
	   finally (return (values suffix pivot)))
      (loop
	 with successor = (- vlen 1)
	 until (> (aref vec successor)
		  (aref vec pivot))
	 do (decf successor)
	 finally (swapf vec pivot successor))
      (reversef vec suffix))))








