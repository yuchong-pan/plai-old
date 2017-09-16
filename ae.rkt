#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
       (rhs AE?)]
  [sub (lhs AE?)
       (rhs AE?)])

;; parse: sexp -> AE
;; to convert s-expression into AEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(and (list? sexp) (= (length sexp 2)))
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(-) (sub (parse (second sexp))
                 (parse (third sexp)))])]))
