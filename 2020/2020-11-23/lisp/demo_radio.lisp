#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; demo_radio.lisp
;;;; Mock internet radio station. 
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

;; Escape sequences for termial
;;(princ (code-char 27)) (princ "[2J") ; Clear Screen
;;(princ (code-char 27)) (princ "[H") ; Home

;; Define variables.
(setf prompt "
	Select Radio Station

	1. The Breeze
	2. Radio Hauraki
	3. RNZ Concert
	
	(Return to Exit)
	
	Enter selection: ")

(setf station (list "Stations" "Breeze" "Hauraki" "Concert"))
;;(format t "~%Station List: ~s" station)

(setf music (list "Music" "dum dee dum" "boom bang boom" "tra la la"))
;;(format t "~%Music Played: ~s" music)

#|
;; Get some info...
(print station) 
(print(length station))
(print (car station))
(print (cdr station))
(print (last station))
(print (nth 0 station))
(print (nth 1 station))
(print (nth 2 station))
(print (nth 3 station))
(print (nth 0 music))
(print (nth 1 music))
(print (nth 2 music))
|#

;; Main loop starts here. 
(loop
	;; Display main menu and get keyboard entry as string to variable a.
	(princ (code-char 27)) (princ "[H") ; Home
	(princ (code-char 27)) (princ "[2J") ; Clear
	(princ prompt) 
	(setq a (read-line))
	;; If Return was pressed then exit.
	(if (< (length a) 1)
		(exit))

	;; Reduce string to just the first character.
	(setf b (subseq a 0 1))
	;;(format t "~%The first character of the string is: ~a" b)

	;; Check is the string is an integer. If so, convert string to int.
    ;; If not, force to be integer of value -1.
	(if (setf c (every #'digit-char-p b))
	    (setf d (parse-integer b))
	    (setf d -1))
	    
	;; Valid menu selections are 1,2 and 3.    
	(if (and (>= d 1) (<= d 3))
		;; Valid. Play the selected music.		
		(format t "~%Station playing is ~s, and the music is ~s." 
			(nth d station)(nth d music))		
		;;Invalid. This can be commented out
		(format t "~%Invalid Selection: ~d " a))	
	
	;; Stay on the radio station until you want to change station.
	(format t "~%~%Press Return Key to continue...")
	(setq x (read-line))
)


