(defpackage #:spst
  (:use #:cl
	#:lisp-unit
	)
  (:export #:rat->contfrac 
	   #:convergents
	   #:split-comma-string
	   #:make-prime-sieve
	   #:make-prime-predicate
	   #:make-distinct-prime-factor-count-sieve
	   #:make-totient-sieve
	   #:binomial 
	   #:number-of-digits
	   #:integer->digit-list
	   #:integer->digit-vector
	   #:digit-seq->integer
	   #:lexically-permutef
	   ))
