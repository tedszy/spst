;;;; sbsl.asd

(asdf:defsystem #:spst
  :description "Simple problem solving tools"
  :author "Ted Szylowiec"
  :license "Specify license here"
  :serial t
  :depends-on (#:lisp-unit
	       #:alexandria
	       #:split-sequence)
  :components ((:file "package")
               (:file "continued-fractions")
	       (:file "sieves")
	       (:file "io")
	       (:file "digits")
	       (:file "permutations")
	       (:file "tuple-trees")))

