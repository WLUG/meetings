#!/usr/bin/clisp
;;;; #!/usr/bin/sbcl --script
;;;; demo_trig.lisp
;;;; Geany Execute for CLISP: clisp "%f" for SBCL: sbcl --script "%f"
;;;; Edit -> Preferences -> Tools -> Terminal: mate-terminal -e "/bin/sh %c"
;;;; Ian Stewart. November 2020. Copyright CC0.

(format t "~%Trigonometric functions supported by COMMON-LISP package:
cos cosh acos acosh sin sinh asin asinh tan tanh atan atanh")

;; Radians to Degrees
(format t "~%Radians to Degrees")

(format t "~%(/ pi 1)_=> ~6f radians. (* (/ pi 1) (/ 180 pi)) => ~6f degrees" 
	(/ pi 1) (* (/ pi 1) (/ 180 pi)))
(format t "~%(/ pi 2)_=> ~6f radians. (* (/ pi 2) (/ 180 pi)) => ~6f degrees" 
	(/ pi 2) (* (/ pi 2) (/ 180 pi)))
(format t "~%(/ pi 3)_=> ~6f radians. (* (/ pi 3) (/ 180 pi)) => ~6f degrees" 
	(/ pi 3) (* (/ pi 3) (/ 180 pi)))
(format t "~%(/ pi 4)_=> ~6f radians. (* (/ pi 4) (/ 180 pi)) => ~6f degrees" 
	(/ pi 4) (* (/ pi 4) (/ 180 pi)))	
(format t "~%(/ pi 6)_=> ~6f radians. (* (/ pi 6) (/ 180 pi)) => ~6f degrees" 
	(/ pi 6) (* (/ pi 6) (/ 180 pi)))		
(format t "~%(/ pi 12)_=> ~6f radians. (* (/ pi 12) (/ 180 pi)) => ~6f degrees" 
	(/ pi 12) (* (/ pi 12) (/ 180 pi)))
(format t "~%(/ pi pi)_=> ~6f radians. (* (/ pi pi) (/ 180 pi)) => ~6f degrees" 
	(/ pi pi) (* (/ pi pi) (/ 180 pi)))
(format t "~%1_=> ~6f radians. (/ 180 pi) => ~6f degrees" 
	1 (/ 180 pi))

;; Cos Sin Tan values
(format t "~%~%Cos Sin Tan values for 30, 45 and 60 degrees")

(format t "~%(cos (/ pi 6)) is cos 30 degrees => ~8f "(cos (/ pi 6)))
(format t "~%(sin (/ pi 6)) is sin 30 degrees => ~8f "(sin (/ pi 6)))		
(format t "~%(tan (/ pi 6)) is tan 30 degrees => ~8f "(tan (/ pi 6)))

(format t "~%(cos (/ pi 4)) is cos 45 degrees => ~8f "(cos (/ pi 4)))
(format t "~%(sin (/ pi 4)) is sin 45 degrees => ~8f "(sin (/ pi 4)))
(format t "~%(tan (/ pi 4)) is tan 45 degrees => ~8f "(tan (/ pi 4)))

(format t "~%(cos (/ pi 3)) is cos 60 degrees => ~8f "(cos (/ pi 3)))
(format t "~%(sin (/ pi 3)) is sin 60 degrees => ~8f "(sin (/ pi 3)))	
(format t "~%(tan (/ pi 3)) is tan 60 degrees => ~8f "(tan (/ pi 3)))

(terpri)(terpri)
;; Pythagorus - Length of hypotenuse
(princ "Use Pythagoras's theorem to calculate the length of the hypotenuse.")
(terpri)
(defvar adjacent)
(princ "Enter length of adjacent: ")
(setq adjacent (read))

(defvar opposite)
(princ "Enter length of opposite: ")
(setq opposite (read))

(format t "~%Length of the hypotenuse: ~d~%" 
	(sqrt(+ (expt adjacent 2)(expt opposite 2))))
	
;;Circle using pi
(terpri)
(princ "Using π to perform calculations.")
(terpri)
(defvar radius)
(princ "Enter the radius: ")
(setq radius (read))

(format t "~%Circumference of circle: ~d" 
	(* 2 pi radius)) ; 2 pi r
(format t "~%Area of circle: ~d" 
	(* pi (expt radius 2))) ; pi r squared
(format t "~%Area of sphere: ~d" 
	(* 4 pi (expt radius 2))) ; 4πr2
(format t "~%Volume of sphere: ~d~%" 
	(* (/ 4 3) pi (expt radius 3))) ; 4/3πr3

(format t "~%~%Press Return Key to Exit")(read-line)
