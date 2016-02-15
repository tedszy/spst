(in-package #:spst)

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


(define-test sieves
  (let ((ps (make-prime-sieve 1000)))
    (assert-equal (loop 
		     for u across (make-prime-sieve 100)
		     summing u)
		  25)
    (let ((g (make-prime-predicate ps)))
      (assert-true (funcall g 641))
      (assert-false (funcall g 231)))
    (let ((dpc (make-distinct-prime-factor-count-sieve ps)))
      (assert-equal (aref dpc 210) 4)
      (assert-equal (aref dpc 30) 3)
      (assert-equal (aref dpc 641) 1))
    (let ((ts (make-totient-sieve ps)))
      (assert-equal (aref ts 100) 40)
      (assert-equal (aref ts 641) 640)
      (assert-equal (aref ts 9) 6)
      (assert-equal (aref ts 1) 1))))
