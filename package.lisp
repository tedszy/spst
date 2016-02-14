(defpackage #:spst
  (:use #:cl
	#:lisp-unit
	)
  (:export #:rational-to-contfrac 
	   #:convergents
	   #:split-comma-string
	   #:join
	   #:join-newline
	   #:make-prime-sieve
	   #:make-prime-predicate
	   #:make-distinct-prime-factor-count-sieve
	   #:make-totient-sieve
	   #:binomial 
	   #:number-of-digits
	   #:integer-to-digit-list
	   #:integer-to-digit-vector
	   #:digit-sequence-to-integer
	   #:lexically-permutef
	   ))
