#!/usr/bin/clisp
;;;; package-symbols.lisp
;;;; Displays sysmbols for known packages
;;;; Tested with: SBCL 2.0.1.debian and CLISP 2.49.92 (2018-02-18)
;;;; The command line supports the argument "function" as a filter.
;;;; By default all symbols are returned, but "function" will return
;;;; symbols that are functions.

;;;; Ian Stewart. November 2020. Copyright CC BY-SA
;;;; The function "all-function-symbols" is
;;;; copyright to Ha-Duong Nguyen 2016. CC BY-SA v4.0 license. 
;;;; http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html

;;; Functions

;; Function to return first command line argument as string
;; If it exists and is "function" the show only the symbols that are functions.
;; http://cl-cookbook.sourceforge.net/os.html 
;; Tested with SBCL and CLISP.
(defun get-argv();
	(or
	#+CLISP *args*
	#+SBCL (cdr *posix-argv*)  
	#+LISPWORKS system:*line-arguments-list*
	#+CMU extensions:*command-line-words*
	nil))

(defvar argv)
;; car returns a string which is the first item of the list.
(setf argv (car (get-argv)))
;;(print argv)
;;(print (type-of argv)) ; (SIMPLE-ARRAY CHARACTER (4))

;; Test command line argument of "function"
;;(if (equalp argv "function") 
;;	(print "Run using getting the symbols that are functions")
;;	(print "Run using all symbols"))
		

;; Get only the function symbols from a package
(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))

;; Get all the symbols from a package
(defun all-symbols (package-name) ;; Returns 1890 for Common-lisp
  "Retrieves all symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
				  ;; removed (and (fboundp symb) to get all symbols
                  (when (eql (symbol-package symb) package) 
                    (push symb lst))) 
                lst)
               (t
                (error "~S does not designate a package" package-name))))))

;; Get the count only of symbols in a package
(defvar symbol-list-count)
(defun all-function-symbols_count (package-name) ;; Returns 1890 for Common-lisp
  "Retrieves a count of all symbols from a package."
    ;;(declare ((or package string symbol) package-name))
	;; Based on command line argv, switch between all-function-symbols and all-symbols
	(if (equalp argv "function") 
		(setq symbol-list-count (all-function-symbols package-name))
		(setq symbol-list-count (all-symbols package-name)))
		
	;; Needs sorting for delete-duplicates to then work OK.
	(dolist (item (sort symbol-list-count #'string-lessp)))
	(delete-duplicates symbol-list-count)
	(length symbol-list-count))

;; Get the symbols in a package with numbering
(defvar counter 0)
(defvar sub-menu)
(defvar symbol-list)
(defun all-function-symbols-list (package-name)	
	;; Call function to get a list of all the symbols
	;; Use command line argv to switch between all-function-symbols and all-symbols
	(if (equalp argv "function") 
		(setq symbol-list (all-function-symbols package-name))	
		(setq symbol-list (all-symbols package-name)))

	;; Sort the symbol-list
	(dolist (item (sort symbol-list #'string-lessp)))	
	(delete-duplicates symbol-list)

	(setq sub-menu '())
	(setf counter 0)
	(dolist (item symbol-list)
		(setf counter (+ counter 1))
		(setf sub-menu (concatenate 'string sub-menu 
		(format nil "~%~5,' d. ~a" counter item))))	
		(setf sub-menu (concatenate 'string sub-menu)))

;;; End of functions
;;; Start of defining variables

;;; Create Main Menu as string variable menu
;; Heading
(defvar menu)
(setf menu "
Lisp Package Reviewer
=====================
Command line argument 'function' filters symbols displayed.")

;; Include Version information and packages count
(setf menu (concatenate 'string menu 
	(format nil "~%Type: ~a" (lisp-implementation-type)) 
	(format nil "~%Version: ~a" (lisp-implementation-version)) 
	(format nil "~%Packages: ~d" (length(list-all-packages)))
	(format nil "~%~%Packages List...~%") 		
	))
	
;; Define a list and initial make it empty. pack-list
(defvar pack-list)
(setq pack-list '())

;; Build a list, "pack-list", of all package names. Note: not in order.
(dolist (item (list-all-packages))
	(setq pack-list (append pack-list (list (package-name item)))))

;; Sort the pack-list
(dolist (item (sort pack-list #'string-lessp)))

;; Create the enumerated list of packages and add to the menu.
;; Use ~3,'0d" for 3 x decimal padded with 0's.
(defvar counter)
(setf counter 0)
(defvar symbol-count)
(setf symbol-count 0)

;; Add counter of how many symbols in each package
(dolist (item pack-list)
	(setf counter (+ counter 1))
	(setf symbol-count (all-function-symbols_count item))
	(setf menu (concatenate 'string menu 
		(format nil "~%~5,' d. ~a ~d" counter item symbol-count))))
		
;; Add the footer to the menu	
(setf menu (concatenate 'string menu 
	(format nil "~%~%       (Return to Exit)~%~%Enter selection: ")))

;; Completed building main menu.
	
;;; End of building variables
;;; Start of main loop

(defvar a)
(defvar b)
(defvar c)
(loop
	;; Display main menu and get keyboard entry as string to variable a.
	(princ (code-char 27)) (princ "[H") ; Home
	(princ (code-char 27)) (princ "[2J") ; Clear
	(princ menu) 
	(setq a (read-line))
	;; If Return was pressed then exit.
	(if (< (length a) 1)
		(exit))

	;; Check is the string is an integer. If so, convert string to int.
	;; And subtract 1 so it is an index into list starting at 0.
    ;; If not an integer, force to be integer of value -1.
	(if (setf b (every #'digit-char-p a))
	    (setf c (- (parse-integer a) 1))
	    (setf c -1))
			
	;; Valid menu selections are 0 length of packages -1    
	(if (and (>= c 0) (< c (length(list-all-packages))))
		;; Valid. Display the symbols for that item in the pack-list		
		(format t "~a" (all-function-symbols-list (nth c pack-list)))		
		;;Invalid.
		(format t "~%Invalid Selection: ~d " a))	

	;; Keep the list displayed until user types Return key
	(format t "~%~%Press Return Key to continue...")
	(read-line)
)

