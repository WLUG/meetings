#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; Output to Console using format
;;;; demo_format.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Output a string to t - the terminal. No quotes displayed
(princ "|")
(format t "Hello World")
(princ "|")

;; Prefix string with a newline. Use ~%
(terpri)
(princ "|")
(format t "~%Hello World")
(princ "|")

;; Prefix and Append string with a newline. Use ~%
(terpri)
(princ "|")
(format t "~%Hello World~%")
(princ "|")

;; Insert integer. Use ~D or ~d for decimal
(format t "~%I am ~D years old." 23)

;; Insert float. Use ~D or ~d for decimal
(format t "~%My foot is ~dcm long." 23.7)

;; Scientific Notation. ~:d
(format t "~%Scientific notation: ~:d" 1000000)

;; Scientific Notation. ~:@d
(format t "~%Scientific notation with plus sign: ~:@d" 1000000)

;; Insert integer. Use ~3,'0d" for 3 x decimal padded with 0's.
(format t "~%My name is James Bond ~3,'0d." 7)

;; Insert name. Use ~a for string. ~3,'0d" for 3 x decimal padded with 0's.
(format t "~%My name is ~a ~3,'0d." "James Bond" 7)

;; Convert to hex with ~x
(format t "~%The value ~d in hex is ~x." 255 255)

;; Convert to octal with ~o
(format t "~%The value ~d in octal is ~o." 255 255)

;; Convert to binary with ~b
(format t "~%The value ~d in binary is ~b." 255 255)

;; Convert to exponential notation with ~e
(format t "~%The value ~d in exponential notation is ~e." 255 255)

;; Convert to fixed length with -f
(format t "~%The value ~d in fixed format is ~7f." 255.123456 255.123456)

;; Convert to dollar and cents with $~$
(format t "~%The value ~d as a price is $~$." 255.1234 255.1234)

;; Print PI to 5 decimal places.
(format t "~%PI to five decimal places: ~5$" pi)

;; Characters with ~c
(format t "~%Character insertion with tilde c: ~c" #\a)
(format t "~%Character insertion with tilde @c: ~@c" #\a)

;; Words. Use ~r 
(format t "~%Number 255: ~r" 255)

;; Roman Numerals. ~@r
(format t "~%Roman Numerals for ~:d: ~@r" 1234 1234)

;; Case manipulation
(format t "~%~(~a~)" "tHe Quick BROWN foX")  
(format t "~%~@(~a~)" "tHe Quick BROWN foX") 
(format t "~%~:(~a~)" "tHe Quick BROWN foX")  
(format t "~%~:@(~a~)" "tHe Quick BROWN foX") 

;; Use ~s when followed by S expression
(format t "~%The value 2 x 3 x 4 is equal to ~s.~%" (* 2 3 4))

;; Display method as part of learning...
(format t "~%(+ 1 2)  =>  ~D" (+ 1 2))
(format t "~%(+ 1 2 3)  =>  ~D" (+ 1 2 3))
(format t "~%(+ 1 2 3 4)  =>  ~D" (+ 1 2 3 4))
(terpri)

(format t "~%~%Press Return Key to Exit")(read-line)
#| 
Summary of main tilded characters.
~A Is followed by ASCII arguments.
~S Is followed by S-expressions.
~D For decimal arguments.
~B For binary arguments.
~O For octal arguments.
~X For hexadecimal arguments.
~C For character arguments.
~F For Fixed-format floating-point arguments.
~E Exponential floating-point arguments.
~$ Dollar and floating point arguments.
~% A new line is printed.
~* Next argument is ignored.
~? Indirection. The next argument must be a string, and the one after it a list.

References: 
http://www.gigamonkeys.com/book/a-few-format-recipes.html
https://www.tutorialspoint.com/lisp/lisp_input_output.htm
https://www.hexstreamsoft.com/articles/common-lisp-format-reference/clhs-summary/#subsections-summary-table

Also:
* (code-char 8)
#\Backspace
* (code-char 9)
#\Tab
* (code-char 10)
#\Newline
* (code-char 12)
#\Page
* (code-char 13)
#\Return
* (code-char 65)
#\A
* (code-char 66)
#\B
* (code-char 27)
#\Esc


|#
