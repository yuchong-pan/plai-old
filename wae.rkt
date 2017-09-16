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
              (parse (third sexp)))])]))

(test (parse '{with {x {+ 5 5}} {+ x x}})
      (with 'x (add (num 5) (num 5)) (add (id 'x) (id 'x))))

;; subst: WAE symbol WAE -> WAE
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [sub (l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? sub-id bound-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v) (if (symbol=? v sub-id) val expr)]))

(test (subst (parse '{+ x {with {x 3} x}}) 'x (num 5))
      (add (num 5)
           (with 'x (num 3) (id 'x))))
(test (subst (parse '{+ x {with {y 3} x}}) 'x (num 5))
      (add (num 5)
           (with 'y (num 3) (num 5))))

;; WAE -> number
;; evaluates WAE expressions by reducing them to numbers

(define (calc expr)
  (type-case WAE expr
    [num (n) n]
    [add (l r) (+ (calc l) (calc r))]
    [sub (l r) (- (calc l) (calc r))]
    [with (bound-id named-expr bound-body)
          (calc (subst bound-body
                       bound-id
                       (num (calc named-expr))))]
    [id (v) (error 'calc "free identifier")]))

(test (calc (parse '5)) 5)
(test (calc (parse '{+ 5 5})) 10)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)
