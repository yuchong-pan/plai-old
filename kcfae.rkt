#lang plai

;; <KCFAE> ::= <num>
;;           | {+ <KCFAE> <KCFAE>}
;;           | {with {<id> <KCFAE>} <KCFAE>}
;;           | <id>
;;           | {fun {<id>} <KCFAE>}
;;           | {<KCFAE> <KCFAE>}
;;           | {if0 <KCFAE> <KCFAE> <KCFAE>}
;;           | {bindcc <id> <KCFAE>}
;;           | {web-read <str>}

(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (fun-expr KCFAE?) (arg-expr KCFAE?)]
  [if0 (test KCFAE?) (truth KCFAE?) (falsity KCFAE?)]
  [bindcc (cont-var symbol?) (body KCFAE?)])

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (p procedure?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value KCFAE-Value?) (next Env?)])

;; lookup : symbol Env -> KCFAE-Value
(define (lookup want env)
  (type-case Env env
    [mtSub () (error "no binding for identifier")]
    [aSub (name value next)
          (if (symbol=? name want)
              value
              (lookup want next))]))

(test/exn (lookup 'x (mtSub)) "no binding for identifier")
(test (lookup 'x (aSub 'x (numV 1) (mtSub))) (numV 1))
(test/exn (lookup 'x (aSub 'y (numV 2) (mtSub))) "no binding for identifier")
(test (lookup 'x (aSub 'x (numV 1) (aSub 'y (numV 2) (mtSub)))) (numV 1))
(test/exn (lookup 'x (aSub 'y (numV 1) (aSub 'z (numV 2) (mtSub)))) "no binding for identifier")

;; parse : sexp -> KCFAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) '+))
         (add (parse (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'with)
              (list? (second sexp)) (= (length (second sexp)) 2) (symbol? (first (second sexp))))
         (app (fun (first (second sexp))
                   (parse (third sexp)))
              (parse (second (second sexp))))]
        [(symbol? sexp) (id sexp)]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'fun)
              (list? (second sexp)) (= (length (second sexp)) 1) (symbol? (first (second sexp))))
         (fun (first (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 2))
         (app (parse (first sexp))
              (parse (second sexp)))]
        [(and (list? sexp) (= (length sexp) 4) (eq? (first sexp) 'if0))
         (if0 (parse (second sexp))
              (parse (third sexp))
              (parse (fourth sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'bindcc) (symbol? (second sexp)))
         (bindcc (second sexp)
                 (parse (third sexp)))]))

(test (parse '1) (num 1))
(test (parse '{+ {+ 3 4} {+ 5 6}}) (add (add (num 3) (num 4))
                                        (add (num 5) (num 6))))
(test (parse '{with {x 3} {+ x x}}) (app (fun 'x (add (id 'x) (id 'x))) (num 3)))
(test (parse 'x) (id 'x))
(test (parse '{fun {x} {+ x x}}) (fun 'x (add (id 'x) (id 'x))))
(test (parse '{{fun {x} {+ x x}} {+ 3 4}}) (app (fun 'x (add (id 'x) (id 'x)))
                                                (add (num 3) (num 4))))
(test (parse '{with {x 0} {if0 x 1 2}}) (app (fun 'x (if0 (id 'x) (num 1) (num 2)))
                                             (num 0)))
(test (parse '{bindcc x {x 3}}) (bindcc 'x (app (id 'x) (num 3))))

;; num+ : KCFAE-Value KCFAE-Value -> KCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

(test (num+ (numV 1) (numV 2)) (numV 3))

;; num-zero? : KCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

(test (num-zero? (numV 1)) #f)
(test (num-zero? (numV 0)) #t)

;; interp : KCFAE Env procedure -> _
(define (interp expr env k)
  (type-case KCFAE expr
    [num (n) (k (numV n))]
    [add (l r)
         (interp l env
                 (lambda (l-val)
                   (interp r env
                           (lambda (r-val)
                             (k (num+ l-val r-val))))))]
    [id (v) (k (lookup v env))]
    [fun (param body)
         (k (closureV (lambda (arg-val dyn-k)
                        (interp body
                                (aSub param
                                      arg-val
                                      env)
                                dyn-k))))]
    [app (fun-e arg-e)
         (interp fun-e env
                 (lambda (fun-val)
                   (interp arg-e env
                           (lambda (arg-val)
                             (type-case KCFAE-Value fun-val
                               [closureV (c) (c arg-val k)]
                               [contV (c) (c arg-val)]
                               [else (error "not an applicable value")])))))]
    [if0 (test truth falsity)
         (interp test env
                 (lambda (test-val)
                   (if (num-zero? test-val)
                       (interp truth env k)
                       (interp falsity env k))))]
    [bindcc (cont-var body)
            (interp body
                    (aSub cont-var
                          (contV (lambda (val)
                                   (k val)))
                          env)
                    k)]))

(test (interp (parse '1) (mtSub) (lambda (x) x)) (numV 1))
(test (interp (parse '{+ {+ 3 4} {+ 5 6}}) (mtSub) (lambda (x) x)) (numV 18))
(test (interp (parse 'x) (aSub 'x (numV 1) (mtSub)) (lambda (x) x)) (numV 1))
(test (interp (parse '{{fun {x} {+ x x}} {+ 3 4}})
              (mtSub)
              (lambda (x) x))
      (numV 14))
(test (interp (parse '{with {x 0}
                            {+ 1
                               {bindcc k
                                       {if0 x
                                            {k 1}
                                            x}}}})
              (mtSub)
              (lambda (x) x))
      (numV 2))
