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

(defun lexically-permutef (vec)
  "In-place lexical permutation of a vector. Using this on 
   a vector that is already at the last lexical permutation
   will throw an out of bounds error."
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

(defun lexically-permute (vec)
  "Non-modifying lexical permutation. Returns a permuted copy
   of the argument vector."
  (let ((v (copy-seq vec)))
    (lexically-permutef v)
    v))

(defun erase-frontf (vec m)
  "Erases elements v[0] up to an including v[m-1], then resizes vector."
  (loop 
     with size = (length vec)
     for k from m below size
     for j from 0
     do 
       (setf (aref vec j) (aref vec k))
     finally 
       (setf (fill-pointer vec)
	     (- size m))))

(defun delete-nthf (sequence n)
  (delete-if (constantly t) sequence :start n :count 1))

(defun nth-permutation (n length)
  "The nth permutation of standard vector #(1 2 ... n)"
  (let ((p (make-array length :adjustable t :fill-pointer 0 :element-type 'fixnum))
	(permuted-p (make-array length :adjustable t :fill-pointer 0 :element-type 'fixnum)))
    (loop 
       for k from 1 to length
       do (vector-push-extend k p))
    (loop
       with i = (1- (length p))
       for d = (factorial i)
       until (= (length p) 0)
       do 
	 (multiple-value-bind (q r)
	     (floor n d)
	   (when (= r 0)
	     (setf r d)
	     (decf q))
	   (vector-push-extend (aref p q) permuted-p)
	   (delete-nth p q)
	   (setf n r)
	   (decf i))
	 finally (return permuted-p))))
	 


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
    (assert-equalp (lexically-permute #(1 2 3 4 5))
		   #(1 2 3 5 4))
    (assert-equalp (nth-permutation 1 10)
		   #(1 2 3 4 5 6 7 8 9 10))
    (assert-equalp (nth-permutation 1000000 10)
		   #(3 8 9 4 10 2 6 5 7 1))
    ))
