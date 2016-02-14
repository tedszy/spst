;;;; sbsl.asd

(asdf:defsystem #:spst
  :description "Simple problem solving tools"
  :author "Ted Szylowiec"
  :license "Specify license here"
  :serial t
  :depends-on (#:lisp-unit)
  :components ((:file "package")
               (:file "spst")
	       (:file "tests")))

