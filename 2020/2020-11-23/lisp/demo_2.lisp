#!/usr/bin/sbcl --script
;;
;; demo_2.lisp
;; Use of shebang
;; $ find /usr -iname sbcl
;; /usr/bin/sbcl

(write-line "Hello world")

(format t "~%Press Return Key to Exit")(read-line)

