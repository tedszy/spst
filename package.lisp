(defpackage #:spst
  (:use #:cl
	#:lisp-unit
	)
  (:export #:rational-to-contfrac 
	   #:convergents
	   #:tenner
	   #:periodic-irrational-contfrac

	   #:split-comma-string
	   #:join
	   #:join-newline

	   #:make-prime-sieve
	   #:make-prime-table
	   #:make-prime-predicate
	   #:make-distinct-prime-factor-count-sieve
	   #:make-totient-sieve
	   #:divisor-pairs

	   #:number-of-digits
	   #:integer-to-digit-list
	   #:integer-to-digit-vector
	   #:digit-sequence-to-integer
	   #:palindromic-p

	   #:factorial
	   #:binomial 
	   #:lexically-permutef
	   #:lexically-permute
	   #:nth-permutation
	   ))
