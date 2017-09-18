#lang plai

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (fun-expr FWAE?) (arg-expr FWAE?)])

;; parse : sexp -> FWAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) '+))
         (add (parse (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'with)
              (list? (second sexp))
              (= (length (second sexp)) 2)
              (symbol? (first (second sexp))))
         (with (first (second sexp))
               (parse (second (second sexp)))
               (parse (third sexp)))]
        [(symbol? sexp) (id sexp)]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'fun)
              (list? (second sexp))
              (= (length (second sexp)) 1)
              (symbol? (first (second sexp))))
         (fun (first (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 2))
         (app (parse (first sexp))
              (parse (second sexp)))]))

(test (parse '3) (num 3))
(test (parse '{+ 3 {+ 4 x}}) (add (num 3) (add (num 4) (id 'x))))
(test (parse '{with {x 3} x}) (with 'x (num 3) (id 'x)))
(test (parse 'x) (id 'x))
(test (parse '{fun {x} {+ x 1}}) (fun 'x (add (id 'x) (num 1))))
(test (parse '{{fun {x} {+ x 1}} {+ 3 4}}) (app (fun 'x (add (id 'x) (num 1)))
                                                (add (num 3) (num 4))))

;; add-numbers : FWAE FWAE -> FWAE
(define (add-numbers a b)
  (num (+ (num-n a) (num-n b))))

(test (add-numbers (num 3) (num 4)) (num 7))

;; subst : FWAE symbol FWAE -> FWAE
(define (subst expr sub-id sub-expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id sub-expr)
                    (subst r sub-id sub-expr))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id sub-expr)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id sub-expr)
                    (subst bound-body sub-id sub-expr)))]
    [id (v)
        (if (symbol=? v sub-id) sub-expr expr)]
    [fun (bound-id bound-body)
         (if (symbol=? bound-id sub-id)
             (fun bound-id bound-body)
             (fun bound-id
                  (subst bound-body sub-id sub-expr)))]
    [app (fun-expr arg-expr)
         (app (subst fun-expr sub-id sub-expr)
              (subst arg-expr sub-id sub-expr))]))

(test (subst (parse '3) 'x (num 5)) (num 3))
(test (subst (parse '{+ 3 {+ x 4}}) 'x (num 5))
      (add (num 3) (add (num 5) (num 4))))
(test (subst (parse '{+ x {with {y 3} x}}) 'x (num 5))
      (add (num 5) (with 'y (num 3) (num 5))))
(test (subst (parse '{+ x {with {x 3} x}}) 'x (num 5))
      (add (num 5) (with 'x (num 3) (id 'x))))
(test (subst (parse 'x) 'x (num 5)) (num 5))
(test (subst (parse 'y) 'x (num 5)) (id 'y))
(test (subst (parse '{fun {y} {+ x y}}) 'x (num 5))
      (fun 'y (add (num 5) (id 'y))))
(test (subst (parse '{fun {x} {+ x x}}) 'x (num 5))
      (fun 'x (add (id 'x) (id 'x))))
(test (subst (parse '{{fun {y} {+ x y}} {+ 3 x}}) 'x (num 5))
      (app (fun 'y (add (num 5) (id 'y))) (add (num 3) (num 5))))

;; interp : FWAE -> FWAE
(define (interp expr)
  (type-case FWAE expr
    [num (n) expr]
    [add (l r) (add-numbers (interp l)
                            (interp r))]
    [with (bound-id named-expr bound-body)
          (interp (subst bound-body
                         bound-id
                         (interp named-expr)))]
    [id (v) (error 'interp "free identifier")]
    [fun (bound-id bound-body) expr]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr))]
           (interp (subst (fun-body fun-val)
                          (fun-param fun-val)
                          (interp arg-expr))))]))

(test (interp (parse '3)) (num 3))
(test (interp (parse '{+ 3 {+ 4 5}})) (num 12))
(test (interp (parse '{with {x 3} {+ x {with {x 4} {+ x x}}}})) (num 11))
(test (interp (parse '{with {x 3} {+ x {with {y 4} {+ x y}}}})) (num 10))
(test (interp (parse '{fun {x} {+ x x}})) (fun 'x (add (id 'x) (id 'x))))
(test (interp (parse '{{fun {x} {+ x x}} {+ 3 4}})) (num 14))
(test (interp (parse '{{{fun {x} x} {fun {x} {+ x 5}}} 3})) (num 8))
(test (interp (parse '{with {x 3} {fun {y} {+ x y}}})) (fun 'y (add (num 3) (id 'y))))

;; pre-process : sexp -> sexp
(define (pre-process sexp)
  (cond [(number? sexp) sexp]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) '+))
         (list '+
               (pre-process (second sexp))
               (pre-process (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'with)
              (list? (second sexp))
              (= (length (second sexp)) 2)
              (symbol? (first (second sexp))))
         (list (list 'fun
                     (list (first (second sexp)))
                     (pre-process (third sexp)))
               (pre-process (second (second sexp))))]
        [(symbol? sexp) sexp]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'fun)
              (list? (second sexp))
              (= (length (second sexp)) 1)
              (symbol? (first (second sexp))))
         (list 'fun
               (list (first (second sexp)))
               (pre-process (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 2))
         (list (pre-process (first sexp))
               (pre-process (second sexp)))]))

(test (pre-process '3) '3)
(test (pre-process '{+ 3 {+ 4 x}}) '{+ 3 {+ 4 x}})
(test (pre-process '{with {x 3} {+ x x}}) '{{fun {x} {+ x x}} 3})
(test (pre-process 'x) 'x)
(test (pre-process '{fun {x} {with {y {+ x x}} {+ y y}}})
      '{fun {x} {{fun {y} {+ y y}} {+ x x}}})
(test (pre-process '{with {x 1} {{fun {y} {+ y x}} 3}})
      '{{fun {x} {{fun {y} {+ y x}} 3}} 1})
