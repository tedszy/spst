(in-package :spst)

(defun number-of-digits (n &key (base 10))
  (if (= n 0)
      1
      (loop until (= n 0)
	 do (setf n (floor n base))
	 counting 1)))

(defun sum-digits (n &key (base 10))
  (loop 
     with s = 0
     until (= n 0)
     do (multiple-value-bind (q r) 
	    (floor n base)
	  (incf s r) 
	  (setf n q))
       finally (return s)))

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

(defun palindromic-p (n)
  (loop 
       with v = (integer-to-digit-vector n)
       with size = (length v)
       for i from 0 to (floor size 2)
       when (not (= (aref v i)
		    (aref v (- size 1 i))))
       do (return nil)
       finally (return t)))

(define-test palindromes
  (assert-true (palindromic-p 121))
  (assert-true (palindromic-p 1))
  (assert-true (palindromic-p 12321))
  (assert-true (palindromic-p 22))
  (assert-true (palindromic-p 1221)))

(define-test integer-digits
  (assert-equal (number-of-digits 0 :base 10) 1)
  (assert-equal (number-of-digits 1234567890 :base 10) 10)
  (assert-equal (number-of-digits 7 :base 2) 3)
  (assert-equal (number-of-digits 17 :base 2) 5)
  (assert-equalp (integer-to-digit-list 12345 :base 10) '(1 2 3 4 5))
  (assert-equalp (integer-to-digit-list 0 :base 10) '(0))
  (assert-equalp (integer-to-digit-vector 0 :base 10) #(0)) 
  (assert-equalp (integer-to-digit-vector 1234 :base 10) #(1 2 3 4))
  (assert-equalp (integer-to-digit-vector 1234832840324 :base 311) 
		 #(131 310 95 286 102))
  (assert-equal (digit-sequence-to-integer #(0) :base 10) 0)
  (assert-equal (digit-sequence-to-integer #(1234)) 1234)
  (assert-equal (digit-sequence-to-integer #(1 1 1) :base 2) 7)
  (assert-equal (digit-sequence-to-integer '(1 2 3 4) :base 10) 1234)
  )

