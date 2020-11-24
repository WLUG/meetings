;;;; 1-lists.lisp

;; I can make a list
(let ((my-numbers (list 1 2 3 4 5)))
  (prin1 my-numbers)
  (terpri))

;; But since everything is a list, I could also just not evaluate some brackets using '
(let ((my-numbers '(1 2 3 4 5)))
  (prin1 my-numbers)
  (terpri))

;;mapping a function to a list (without copying it)
;;To refer to a symbol's function, there is #'
(let ((my-numbers '(1 2 3 4 5)))
  (mapc #'print my-numbers))

;;Apply a function, creating a new copy of the list
;;1+ is a function that adds one
(let ((my-numbers '(1 2 3 4 5)))
  (prin1 (mapcar #'1+ my-numbers))
  (terpri))

;; Mapping functions generally accept lots of lists,
;; as long as the function makes sense.
(let ((my-numbers '(1 2 3 4 5)))
  (prin1
   (mapcar #'+ my-numbers my-numbers))
  (terpri))

;; Often we want to provide an ad hoc function by writing a lambda
;; Since lambdas are their own names, we don't need #' (though it wouldn't mind)
;; #'cond chooses what to do from a list of (predicate form)s.
;; t is the default form to use.
(let ((my-numbers '(1 2 3 4 5)))
  (prin1
   (mapcar (lambda (x) (cond ((evenp x) (1+ x))
			     ((oddp x) (1- x))
			     (t (error "number wasn't even or odd"))))
	   my-numbers))
  (terpri))

;; Copy a list, with non-even members removed
(let ((my-numbers '(1 2 3 4 5)))
  (prin1
   (remove-if-not #'evenp my-numbers))
  (terpri))

;;; cons
;; lisp linked lists are made by consing
;; (allocating memory for an element of a list).
;; This is a low level concept, but precisely because lisp is garbage collected
;; we like to be aware of our memory allocation.
(let ((element-1 (cons 1 nil))
      (same-as-1 (list 1)))
  (prin1 element-1)
  (prin1 same-as-1)
  (terpri))

;; A cons has two pointers.
;; car, which points to its value
;; cdr, which points to a next element (which is nil if the list ended)
(let ((element-1 (cons 1 nil)))
  (prin1 (cons 0 element-1))
  (terpri))

;; These are setf-able. Be careful about looping your linked lists though.
(let ((element-1 (cons 1 nil))
      (element-2 (cons 2 nil))
      (element-3 (cons 3 nil)))
  (prin1 element-1)
  (terpri)
  (prin1 element-2)
  (terpri)
  (prin1 element-3)
  (terpri)

  ;;Setting the pointer to the next element
  (setf (cdr element-1) element-2)

  (prin1 element-1)
  (terpri)
  (prin1 element-2)
  (terpri)

  (setf (cdr element-2) element-3)
  (prin1 element-1)
  (terpri)

  ;;Danger zone
  (setf (cdr element-3) element-1)
  
  (setf *print-circle* t)
  
  (print element-2))
