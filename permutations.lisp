(in-package #:spst)

(defun factorial (n)
  (loop 
     for j from 1 to n
     with result = 1  
     do (setf result (* result j))
     finally (return result)))

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


(define-test binomial-factorial
  (assert-equal (factorial 5) 120)
  (assert-equal (factorial 1) 1)
  (assert-equal (factorial 0) 1)
  (assert-equal (binomial 49 6) 13983816)
  (assert-equal (binomial 0 0) 1)
  (assert-equal (binomial 1 1) 1)
  (assert-equal (binomial 3 5) 0)
  (assert-equal (binomial 10 10) 1)
  (assert-equal (binomial 23 12) 1352078)
  (assert-equal (binomial 10 6) 210)
 )

(define-test vector-permutations
  (let ((foo (vector 1 2 3 4))
	(goo (vector 0 1 2 3 4 5 6 7 8 9))
	(soo (vector 0 1 2 5 3 3 0)))
    (swapf foo 1 3)
    (assert-equalp foo #(1 4 3 2))
    (reversef goo 5)
    (assert-equalp goo #(0 1 2 3 4 9 8 7 6 5))
    (lexically-permutef soo)
    (assert-equalp soo #(0 1 3 0 2 3 5))
    ))
