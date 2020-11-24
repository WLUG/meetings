;;;; 2-closures.lisp

;;; Instead of juggling variables,
;; it is useful to use functions to access or update values.
;; closures in lisp have dynamic scope and infinite extent
;; which is to say they you can pass them around happily and
;; they won't get garbage collected until they are no longer reachable.

(defun make-counter (wrap-number)
  (let ((count 0))
    (lambda () (mod (incf count) wrap-number))))

(let ((counter (make-counter 4)))
  (prin1 (loop repeat 5 collect (funcall counter))))

;; When experimenting, it is more convenient not to worry about global scope
;; so I think you can happily use defvar and setq

(defvar counter)
(setq counter (make-counter 2))

(funcall counter)
(funcall counter)
(funcall counter)

;; To use it like a function, you need to set the symbol's symbol-function

(setf (symbol-function 'counter) counter)

(loop repeat 5 do (print (counter)))
