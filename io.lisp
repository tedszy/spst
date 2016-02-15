(in-package #:spst)

;; Strings.

(defun split-comma-string (str)
  (loop 
     for u across str
     with result = nil
     with v = (make-array 0 :fill-pointer 0 :element-type 'base-char)
     if (char-equal u #\,) 
     do 
       (push (copy-seq v) result)
       (setf (fill-pointer v) 0)
     else do 
       (vector-push-extend u v)
     finally 
       (push v result)
       (return (reverse result))))

(defun join (out list-of-strings delimiter)
  "If you use t for the output stream, it goes to standard output. 
   And you can use \"~%\" for the delimiter to get items separated 
   by newlines."
  (let ((fmt (format nil "~a~a~a" "~{~a~^" delimiter "~}")))
    (format out fmt list-of-strings)))

(defun join-newline (out list-of-strings)
  "Convenience function. Joins a list of strings with a newline separator."
  (join out list-of-strings "~%"))


(define-test strings
    (assert-equal (split-comma-string "a,bb,ccc,dddd")
		  (list "a" "bb" "ccc" "dddd"))
    (assert-equal (join nil '("a" "bb" "cc") "--")
		  "a--bb--cc")
    (assert-equal (join-newline nil '("a" "bb" "cc"))
		  (format nil "~a~%~a~%~a" "a" "bb" "cc"))
  )
