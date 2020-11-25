;;;; 4-saving.lisp

;;;; NB every line that saves a core / executable quits the lisp REPL

;;; Assuming you are using SBCL, though everyone is kind of similar.



;;; Saving and loading a core (with only one thread running)

;;; In your REPL:
(sb-ext:save-lisp-and-die "my-core")
;;; In your shell:
;; sbcl --core my-core

					; or
;;; In your REPL:
(sb-ext:save-lisp-and-die "my-core" :executable t)

;;; In your shell:
;; ./my-core


;;; Saving a binary that executes a function instead of the REPL

;;; In your REPL:
(defun hello-function () (format t "hello world~%"))
(sb-ext:save-lisp-and-die "hello-world" :executable t :toplevel #'hello-function)

;;; In your shell:
;; ./hello-world


;;; Basic command line arguements list
(defun print-arguements () (format t "Arguements:~%~{~a~%~}" (uiop:command-line-arguments)))
(sb-ext:save-lisp-and-die "list-arguements" :executable t :toplevel #'print-arguements)

;;; In your shell:
;; ./list-arguements foo bar baz
;; Arguements:
;; foo
;; bar
;; baz



;;;; GOTCHAS

;;; 1.
;;; The popular emacs REPL mode, slime, itself runs as two threads.
;;; So you can't save the REPL normally.

;;; 2.
;;; Quicklisp by default puts itself in your lisp user init file (.sbclrc).
;;; ; It makes your executables not portable if it's hiding in there.
;;; --no-userinit if you ever want to save your work?
;;; ; But then you can't use quicklisp or your .sbclrc for anything.
;;; (delete-package 'quicklisp) after using it
;;; ; fine, I guess.

					;This section is like talking
					;about LD_LIBRARY_PATH for lisp
;;; 3.
;;; Quicklisp installs packages into path/to/quicklisp/quicklisp/dists/software/
;;; Systems are meant to be in ~/common-lisp/ or ~/.local/share/common-lisp/
;;; which are the places that common lisp looks by default.
;;; ; symlinking there is fine
;;; ln -s quicklisp/quicklisp/dists/software/a-system ~/.local/share/common-lisp/
;;; ; but seems like more work.
;;; Customise where asdf/quicklisp locations? Non-portable.
;;; The maker of quicklisp maintains a popular app in most package managers,
;;; 'buildapp' for building common lisp systems outside of the REPL.

;;; 4.
;;; Starting with a template for the system of files for defining a package
;;; ; quicklisp guy has a package, quickproject that's quite convenient.

