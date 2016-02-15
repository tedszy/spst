(in-package :spst)



(define-test strings
    (assert-equal (split-comma-string "a,bb,ccc,dddd")
		  (list "a" "bb" "ccc" "dddd"))
    (assert-equal (join nil '("a" "bb" "cc") "--")
		  "a--bb--cc")
    (assert-equal (join-newline nil '("a" "bb" "cc"))
		  (format nil "~a~%~a~%~a" "a" "bb" "cc"))
  )


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

(define-test binomial
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

