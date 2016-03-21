(in-package :spst)

(defun subsets (list)
  (if (null list)
      '(nil)
      (let ((item (car list))
	    (subsets (subsets (cdr list))))
	(append subsets (loop
			   for set in subsets 
			   collect (cons item set))))))

;; combinations of length n taken from list s
(defun n-combinations (s n)
  (cond ((zerop n) 
	 (list nil))
        (t 
	 (loop 
	    for l on s
	    nconc (loop 
		     for c in (n-combinations (cdr l) (1- n))
		     collect (cons (car l) c))))))
