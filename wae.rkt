#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

;; parse : sexp -> WAE
;; to convert s-expression to WAEs

(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(and (list? sexp) (= (length sexp) 3))
     (cond
       [(eq? (first sexp) '+)
        (add (parse (second sexp))
             (parse (third sexp)))]
       [(eq? (first sexp) '-)
        (sub (parse (second sexp))
             (parse (third sexp)))]
       [(and (eq? (first sexp) 'with)
             (list? (second sexp))
             (symbol? (first (second sexp))))
        (with (first (second sexp))
              (parse (second (second sexp)))
              (parse (third sexp)))]
       [else
        (error "parse error")])]
    [else
     (error "parse error")]))

(test (parse '{with {x {+ 5 5}} {+ x x}})
      (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))
