#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; Output to Console
;;;; demo_print.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Print. Inserts a newline prior to outputting quoted string
(princ "|")
(print "Hello world using print") 
(princ "|")

;; Princ. Output, without quotes, just the string
(terpri) ; Output a newline
(princ "|")
(princ "Hello world using princ")
(princ "|")

;; Prin1. Output, without quotes, just the string
(terpri) ; Output a newline
(princ "|")
(prin1 "Hello world using prin1")
(princ "|")

;; write Outputs quoted string
(terpri)
(princ "|")
(write "Hello world using write")
(princ "|")

;; write-line. Output, without quotes, string and then add a newline
(terpri)
(princ "|")
(write-line "Hello World using write-line") 
(princ "|")

;; write-string. Output, without quotes, just the string
(terpri)
(princ "|")
(write-string "Hello World using write-string") 
(princ "|")

;; write-char. Single characters including special character Tab.
;; Reference: http://clhs.lisp.se/Body/f_wr_cha.htm
(terpri)
(princ "|")
(write-char #\H)
(write-char #\e)
(write-char #\l)
(write-char #\Tab)
(write-char #\l)
(write-char #\o)
(princ "|")
(terpri)

(format t "~%~%Press Return Key to Exit")(read-line)

#|
Special Characters
Common LISP allows using the following 
special characters in your code: 

    #\Backspace
    #\Tab
    #\Linefeed
    #\Page
    #\Return
    #\Rubout
    
They are called the semi-standard characters.    
|#

