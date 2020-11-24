;;;;let

;;; let locally binds pairings of symbols and parameters.
(let ((variable-1 "hello world"))
  (princ variable-1)
  (terpri))

;;; using a let to locally change the binding of a symbol
(let ((variable-1 "hello")
      (variable-2 "world"))
  (princ variable-1)
  (terpri)
  
  (let ((variable-1 (concatenate 'string variable-1 " " variable-2)))
    (princ variable-1)
    (terpri))
  
  (princ variable-2)
  (princ " ")
  (princ variable-1)
  (terpri))


;;;;setf
;;; setf sets what getters get. If a getter is possible to set, the compiler will figure out how to do it.

;;; This could be a variable and its value
(defvar foo 'baz)
(princ foo)
(terpri)

(setf foo 'bar)
(princ foo)
(terpri)


;;;;lambda

;;lambda forms in lisp are their own names, so you can treat them like functions
((lambda () (princ "hello world") (terpri)))

;;I can put a lambda form in the value of a symbol
(defvar hello-world (lambda () (princ "hello world") (terpri)))

;;Funcall evaluates a value as a function (which it hopefully is)
(funcall hello-world)

;; setf-ing the function of a symbol so it can be used as a function
(setf (symbol-function 'hello-world)
      (lambda () (princ "hello world") (terpri)))

(hello-world)

;;cf defun, which defines functions
(defun hello-world () (princ "hello world") (terpri))
(hello-world)

(setf (symbol-function 'hello-world)
      (lambda () (princ "world hello") (terpri)))
(hello-world)
