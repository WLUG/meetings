;;;; 3-REPL.lisp

;;; Lisp's REPL

#|
* ** *** are the results of your last, second last and third last commands
+ ++ +++ are your last, second last and third last commands
|#

;; Save a command, after running it:
;; Note that * is also the prompt.
#|
* (defvar *saved-commands* (list))
*SAVED-COMMANDS*
* (1+ 1)
2
* (push + *saved-commands*)
((1+ 1))
* (eval (first *saved-commands*))
2
* ***
2
|#


;;; Errors

;;; Spelling error?
(let ((first-var 1)
      (second-var 2))
  (+ first-var second-vra))

#|
restarts (invokable by number or by possibly-abbreviated name):
  0: [CONTINUE   ] Retry using SECOND-VRA.
  1: [USE-VALUE  ] Use specified value.
  2: [STORE-VALUE] Set specified value and use it.
  3: [ABORT      ] Exit debugger, returning to top level.

((LAMBDA ()))
   source: (+ FIRST-VAR SECOND-VRA)
0] (setf second-vra 2)			; I entered this
2
0] 0					; then this
3
|#
					; If an error has happened,
					; the default is to give you
					; the opportunity to fix it
					; directly how you like.
					;(useful!)

;;; Consciously ignoring (but still printing) errors
(ignore-errors
  (let ((first-var 1)
	(second-var 2))
    (+ first-var second-vra)))

;;; Customising the debugger hook to print what went wrong
;;; and then abort debugging
(setf *debugger-hook* (lambda (&rest args) (prin1 (first args)) (abort)))


;;; Feature rich (autocomplete and readline / multiline) REPL
;;; https://www.common-lisp.net/project/linedit/
