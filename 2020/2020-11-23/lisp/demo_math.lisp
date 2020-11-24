#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; demo_math.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Display the function as part of the learning...
(princ "Addition.")
(format t "~%(+ 1 2)  =>  ~D" (+ 1 2))
(format t "~%(+ 1 2 3)  =>  ~D" (+ 1 2 3))
(format t "~%(+ 1 2 3 4)  =>  ~D~%" (+ 1 2 3 4))

(princ "Multiply.")
(format t "~%(* 1 2 3 4)  =>  ~D~%" (* 1 2 3 4))

(princ "Absolute.")
(format t "~%(abs -5)  =>  ~D~%" (abs -5))

(princ "Modulus.")
(format t "~%(mod 6 3)  =>  ~D" (mod 6 3))
(format t "~%(mod 7 3)  =>  ~D" (mod 7 3))
(format t "~%(mod 8 3)  =>  ~D" (mod 8 3))
(format t "~%(mod 9 3)  =>  ~D~%" (mod 9 3))

(princ "Remainder.")
(format t "~%(rem 6 3)  =>  ~D" (rem 6 3))
(format t "~%(rem 7 3)  =>  ~D" (rem 7 3))
(format t "~%(rem 8 3)  =>  ~D" (rem 8 3))
(format t "~%(rem 9 3)  =>  ~D~%" (rem 9 3))

(princ "Floor.")
(format t "~%(floor 5 3)  =>  ~D" (floor 5 3))
(format t "~%(floor 6 3)  =>  ~D" (floor 6 3))
(format t "~%(floor 7 3)  =>  ~D" (floor 7 3))
(format t "~%(floor 8 3)  =>  ~D" (floor 8 3))
(format t "~%(floor 9 3)  =>  ~D~%" (floor 9 3))

(princ "Ceiling.")
(format t "~%(ceiling 5 3)  =>  ~D" (ceiling 5 3))
(format t "~%(ceiling 6 3)  =>  ~D" (ceiling 6 3))
(format t "~%(ceiling 7 3)  =>  ~D" (ceiling 7 3))
(format t "~%(ceiling 8 3)  =>  ~D" (ceiling 8 3))
(format t "~%(ceiling 9 3)  =>  ~D~%" (ceiling 9 3))

; Usually for division to integer TRUNCATE is used?
(princ "Truncate.")
(format t "~%(truncate 5 3)  =>  ~D" (truncate 5 3))
(format t "~%(truncate 6 3)  =>  ~D" (truncate 6 3))
(format t "~%(truncate 7 3)  =>  ~D" (truncate 7 3))
(format t "~%(truncate 8 3)  =>  ~D" (truncate 8 3))
(format t "~%(truncate 9 3)  =>  ~D~%" (truncate 9 3))

(princ "Exponential function.")
(format t "~%(expt 2 2)  =>  ~D" (expt 2 2))
(format t "~%(expt 2 3)  =>  ~D" (expt 2 3))
(format t "~%(expt 2 4)  =>  ~D" (expt 2 4))
(format t "~%(expt 2 5)  =>  ~D~%" (expt 2 5))

(princ "Natural Exponential function.")
(format t "~%(exp 1)  =>  ~D" (exp 1))
(format t "~%(exp 2)  =>  ~D" (exp 2))
(format t "~%(exp 3)  =>  ~D" (exp 3))
(format t "~%(exp 4)  =>  ~D~%" (exp 4))

(princ "gcd - Greatest Common Divisor.")
(format t "~%(gcd 18 9 6)  =>  ~D~%" (gcd 18 9 6))

(princ "Division.")
(format t "~%(/ 6 2)  => ~s" (/ 6 2))
(format t "~%(/ 7 2)  => ~s" (/ 7 2))
(format t "~%(/ 7.0 2)  =>  ~s" (/ 7.0 2))
(format t "~%(/ 7 2.0)  =>  ~s~%" (/ 7 2.0))

(princ "Round.")
(format t "~%(round 4.5)  => ~s" (round 4.5))
(format t "~%(round 5.5)  => ~s" (round 5.5))
(format t "~%(round 6.5)  => ~s" (round 6.5))
(format t "~%(round 7.5)  => ~s~%" (round 7.5))

(princ "Square Root.")
(format t "~%(sqrt 2)  => ~s" (sqrt 2))
(format t "~%(sqrt 9)  => ~s~%" (sqrt 9))

(princ "Integer Square Root.")
(format t "~%(isqrt 15)  => ~s" (isqrt 15))
(format t "~%(isqrt 16)  => ~s" (isqrt 16))
(format t "~%(isqrt 17)  => ~s~%" (isqrt 17))

(princ "Natural Logarithm.")
(format t "~%(log 2.718281828459) => ~s" (log 2.718281828459))
(format t "~%(log 7.389056)  => ~s~%" (log 7.389056))

(terpri)
(princ "Random. Ten integers between 0 and 9 inclusive.")(terpri)
(princ "(print (loop repeat 10 collect (random 10))) =>")
(print (loop repeat 10 collect (random 10)))
(terpri)(terpri)
(princ "Random. Five floats between from 0 and less than 1.")(terpri)
(princ "(print (loop repeat 5 collect (random 1.0))) =>")
(print (loop repeat 5 collect (random 1.0)))

(terpri)(terpri)
(princ "Random. Ten integers between 10 and 19 inclusive.")(terpri)
(princ "(print (loop repeat 10 collect (+ 10 (random 10)))) =>")
(print (loop repeat 10 collect (+ 10 (random 10))))

(terpri)(terpri)
(princ "Using multiple-value-bind, to return two values")(terpri)
(defvar message)  ;;<-- need this for SBCL but not CLISP
(setf message "
Example Floor Division with Dividend: 13. Divisor: 3.
Returns the Quotent and the Modulus...
(multiple-value-bind (quotient modulus)
   (floor 13 3)
   (format t \"~&	Quotient: ~d\" quotient)
   (format t \"~&	Modulus: ~d\" modulus))
")
(princ message)

(terpri)
(princ "Floor Division. Dividend: 13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (floor 13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)(terpri)
(princ "Ceiling Division. Dividend: 13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (ceiling 13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)(terpri)
(princ "Truncate Division. Dividend: 13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (truncate 13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)(terpri)
(princ "Floor Division. Dividend: -13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (floor -13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)(terpri)
(princ "Ceiling Division. Dividend: -13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (ceiling -13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)(terpri)
(princ "Truncate Division. Dividend: -13. Divisor: 3.")
(multiple-value-bind (quotient modulus)
   (truncate -13 3)
   (format t "~&	Quotient: ~d" quotient)
   (format t "~&	Modulus: ~d" modulus))
(terpri)

(format t "~%~%Press Return Key to Exit")(read-line)

#| 
Checked Remainder (rem). Only returning one value of modulus.
https://www.tutorialspoint.com/lisp/lisp_numbers.htm
|#
