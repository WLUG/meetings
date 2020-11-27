#!/usr/bin/sbcl --script
;+
; Emit integer triples (a, b, c) such that a² + b² = c².
; From <http://www.mathsisfun.com/numbers/pythagorean-triples.html>,
; given any two positive integers m and n, we can construct a
; Pythagorean triple by
;
;     a = n² - m²
;     b = 2nm
;     c = n² + m²
;
; Written 2020 November 27 by Lawrence D'Oliveiro <ldo@geek-central.gen.nz>.
;-

(defun pythag-triple (n m)
    "returns a pythagorean triple (a, b, c) from the given n, m."
    (list (- (* n n) (* m m)) (* 2 n m) (+ (* n n) (* m m)))
) ; pythag-triple

(defvar n 1)
(loop
    (do ((m 1 (+ m 1))) ((>= m n))
        (let (a b c)
            (multiple-value-setq (a b c) (values-list (pythag-triple n m)))
            (when (eq (gcd a b c) 1)
                (format t "(~A, ~A) => (~A, ~A, ~A)~%" n m a b c)
            ) ; when
        ) ; let
    ) ; do
    (setq n (+ n 1))
    ;;(read-line)
) ; loop
