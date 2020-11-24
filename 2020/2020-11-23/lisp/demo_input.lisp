#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; Input to Console
;;;; demo_input.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Input from Console. May execute out-of-order on SBCL?
(print "Input from the console")
(terpri)
(terpri)
(princ "Using read. Enter your first name: ")
(princ(read))

(terpri)
(princ "Using read-line. Enter your surname: ")
(princ(read-line))

;; Using a function to read the input
(terpri)
(print "Using a function to Read input from console")
(terpri)
(defvar a)
(defun get-console-input ()
	(princ "Enter some data: ")
	(setq a (read))(princ a))
    ;;(princ(setq a (read))))
  
;; Call function
(get-console-input)


;; Using a function with no global / dynamic variables
(terpri)
(print "Using a function with no global variables")(terpri)
(defun read-&-format ()
  "Reads 3 numbers and prints a line with their sum" ;; for (documentation)
  (flet ((prompt (string)
           (format t "~a: " string)
           (finish-output)
           (read nil 'eof nil)))
    (let ((x (prompt "First number"))
          (y (prompt "Second number"))
          (z (prompt "Third number")))
      (format t "~&The sum of ~a, ~a, & ~a is: ~a~%"
              x y z (+ x y z)))))

;;Evaluate the above function definition, then run the form:
(read-&-format)
(format t "~%Display the documentation on the function (read-&-format)")
(print(documentation 'read-&-format 'function))

(format t "~%~%Press Return Key to Exit")(read-line)
#|
Original of code example above. From:
https://stackoverflow.com/questions/26171913/how-to-read-user-input-in-lisp

;; Using a function with no variables
(terpri)
(print "Using a function with no variables")(terpri)
(defun read-3-numbers-&-format-sum ()
  (flet ((prompt (string)
           (format t "~a: " string)
           (finish-output)
           (read nil 'eof nil)))
    (let ((x (prompt "First number"))
          (y (prompt "Second number"))
          (z (prompt "Third number")))
      (format t "~&The sum of ~a, ~a, & ~a is: ~a~%"
              x y z (+ x y z)))))

;;Evaluate the above function definition, then run the form:
(read-3-numbers-&-format-sum)

Notes:
http://www.gigamonkeys.com/book/variables.html

Common Lisp supports two kinds of variables: lexical and dynamic.
These two types correspond roughly to "local" and "global" variables in 
other languages. 

|#

