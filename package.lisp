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
	   #:sum-digits
	   #:integer-to-digit-list
	   #:integer-to-digit-vector
	   #:digit-sequence-to-integer
	   #:palindromic-p

	   #:factorial
	   #:binomial 
	   #:lexically-permutef
	   #:lexically-permute
	   #:nth-permutation

	   #:pythagorean-tuple
	   #:pythagorean-tuple-with-path
	   #:make-pytuple
	   #:make-pytuple-with-path
	   #:tuple=
	   #:valid-p
	   #:barning-mul
	   #:depth-search-pytree
	   #:follow-path

	   #:subsets
	   #:n-combinations

	   #:graph
	   #:graph.nodes
	   #:graph.edges
	   #:graph.num-nodes
	   #:graph.num-edges
	   #:graph-with-weighted-nodes
	   #:graph-with-weighted-edges
	   #:make-graph-with-weighted-edges
	   #:make-graph-with-weighted-nodes
	   #:bellman-ford
	   #:optimal-solution
	   ))
