#lang plai

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  [if0 (condition F1WAE?) (true-clause F1WAE?) (false-clause F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value number?) (ds DefrdSub?)])

;; parse : sexp -> F1WAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(symbol? sexp) (id sexp)]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) '+))
         (add (parse (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'with)
              (list? (second sexp))
              (symbol? (first (second sexp))))
         (with (first (second sexp))
               (parse (second (second sexp)))
               (parse (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 2)
              (symbol? (first sexp)))
         (app (first sexp)
              (parse (second sexp)))]
        [(and (list? sexp)
              (= (length sexp) 4)
              (eq? (first sexp) 'if0))
         (if0 (parse (second sexp))
              (parse (third sexp))
              (parse (fourth sexp)))]))

(test (parse '3) (num 3))
(test (parse 'x) (id 'x))
(test (parse '{+ 3 4}) (add (num 3) (num 4)))
(test (parse '{with {x 5} {+ x x}}) (with 'x (num 5) (add (id 'x) (id 'x))))
(test (parse '{f 5}) (app 'f (num 5)))
(test (parse '{if0 0 1 2}) (if0 (num 0) (num 1) (num 2)))
(test (parse '{if0 {+ 3 4} 1 2}) (if0 (add (num 3) (num 4)) (num 1) (num 2)))
(test (parse '{with {x 5} {if0 {+ x -5} 0 {double x}}})
      (with 'x (num 5) (if0 (add (id 'x) (num -5)) (num 0) (app 'double (id 'x)))))

;; subst : F1WAE symbol F1WAE -> F1WAE

(define (subst expr sub-id val)
  (type-case F1WAE expr
    [num (n) expr]
    [add (l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [with (bound-id named-expr bound-body)
          (if (symbol=? bound-id sub-id)
              (with bound-id
                    (subst named-expr sub-id val)
                    bound-body)
              (with bound-id
                    (subst named-expr sub-id val)
                    (subst bound-body sub-id val)))]
    [id (v)
        (if (symbol=? v sub-id) val expr)]
    [app (fun-name arg-expr)
         (app fun-name
              (subst arg-expr sub-id val))]
    [if0 (condition true-clause false-clause)
         (if0 (subst condition sub-id val)
              (subst true-clause sub-id val)
              (subst false-clause sub-id val))]))

(test (subst (parse '{+ 3 4}) 'x (num 5)) (add (num 3) (num 4)))
(test (subst (parse '{+ x 4}) 'x (num 5)) (add (num 5) (num 4)))
(test (subst (parse '{with {y 3} {+ x y}}) 'x (num 5)) (with 'y (num 3) (add (num 5) (id 'y))))
(test (subst (parse '{+ x {with {x 3} x}}) 'x (num 5)) (add (num 5) (with 'x (num 3) (id 'x))))
(test (subst (parse 'x) 'x (num 5)) (num 5))
(test (subst (parse 'y) 'x (num 5)) (id 'y))
(test (subst (parse '{f {+ 3 x}}) 'x (num 5)) (app 'f (add (num 3) (num 5))))
(test (subst (parse '{if0 x -1 {+ x x}}) 'x (num 5)) (if0 (num 5) (num -1) (add (num 5) (num 5))))

;; lookup-fundef : symbol listof(FunDef)
(define (lookup-fundef fun-name fundefs)
  (cond [(empty? fundefs)
         (error fun-name "function not found")]
        [(symbol=? fun-name (fundef-fun-name (first fundefs)))
         (first fundefs)]
        [else
         (lookup-fundef fun-name (rest fundefs))]))

(test (lookup-fundef 'f (list (fundef 'f 'x (add (id 'x) (id 'x)))
                              (fundef 'g 'y (with 'x (num 5) (add (id 'y) (id 'x))))))
      (fundef 'f 'x (add (id 'x) (id 'x))))

;; lookup : symbol DefrdSub -> number
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))

(test (lookup 'x (aSub 'y 3 (aSub 'x 5 (mtSub)))) 5)

;; interp : F1WAE listof(FunDef) DefrdSub -> number
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
    [num (n) n]
    [add (l r) (+ (interp l fun-defs ds)
                  (interp r fun-defs ds))]
    [with (bound-id named-expr bound-body)
          (interp bound-body
                  fun-defs
                  (aSub bound-id
                        (interp named-expr fun-defs ds)
                        ds))]
    [id (v) (lookup v ds)]
    [app (fun-name arg-expr)
         (local [(define the-fun-def (lookup-fundef fun-name fun-defs))]
           (interp (fundef-body the-fun-def)
                   fun-defs
                   (aSub (fundef-arg-name the-fun-def)
                         (interp arg-expr fun-defs ds)
                         (mtSub))))]
    [if0 (condition true-clause false-clause)
         (if (zero? (interp condition fun-defs ds))
             (interp true-clause fun-defs ds)
             (interp false-clause fun-defs ds))]))

(test (interp (parse '5) '() (mtSub)) 5)
(test (interp (parse '{+ 3 4}) '() (mtSub)) 7)
(test (interp (parse '{with {x 3} {+ x x}}) '() (mtSub)) 6)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}}) '() (mtSub)) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}}) '() (mtSub)) 10)
(test (interp (parse '{with {x 5} {f x}})
              (list (fundef 'f 'x (add (id 'x) (id 'x))))
              (mtSub))
      10)
(test (interp (parse '{double {double 5}})
              (list (fundef 'double
                            'n
                            (add (id 'n) (id 'n))))
              (mtSub))
      20)
(test (interp (parse '{sum 5})
              (list (fundef 'sum
                            'n
                            (if0 (id 'n)
                                 (num 0)
                                 (add (id 'n) (app 'sum (add (id 'n) (num -1)))))))
              (mtSub))
      15)
