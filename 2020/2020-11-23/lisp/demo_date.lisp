#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; demo_date.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

(format t "Off to sleep for one second using the function (sleep 1)...~%")
(sleep 1)
(format t "Finished sleeping.~%")

(format t "~%Use (get-internal-real-time) to return micro-seconds 
since 1 Jan 1970: ~s~%" (get-internal-real-time))

(format t "~%Return seconds since epoch: ~:d~%" 
	(floor (get-internal-real-time) 1000000))

(format t "~%Using mutliple-value-bind to truncate (get-internal-real-time) 
as scientific notation with float.")
(multiple-value-bind
	(sec microsec)
    (truncate (get-internal-real-time) 1000000)
    (format t "~%Seconds and microseconds since Linux epoch: ~:d.~d~%" 
		sec microsec))       
           

(format t "~%Get date and time using (multiple-value-bind):~%")

(defconstant *day-names*
	'("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
*DAY-NAMES*

(multiple-value-bind
	(second minute hour day month year day-of-week dst-p tz)
	(get-decoded-time)
	(format t "~%It is now ~2,'0d:~2,'0d:~2,'0d on ~a, ~0d/~2,'0d/~d (GMT~@d)~%"
		hour
		minute
		second
	    (nth day-of-week *day-names*)
	    month
	    day
		year
		(- tz)))

(format t "~%Get time HH:MM using (multiple-value-bind):~%")
(multiple-value-bind
	(second minute hour day month year day-of-week dst-p tz)
	(get-decoded-time)
	(format t "~%It is now ~2,'0d:~2,'0d.~%" hour minute)) 

(format t "~%Press Return Key to Exit")(read-line)
    	  
#|
Notes:   	   
http://cl-cookbook.sourceforge.net/dates_and_times.html
https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
(print(GET-INTERNAL-RUN-TIME))  ;;7624  ; 12118 huh?
	
|#
