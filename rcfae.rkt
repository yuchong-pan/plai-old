#lang plai

;; <RCFAE> ::= <num>
;;           | {+ <RCFAE> <RCFAE>}
;;           | {* <RCFAE> <RCFAE>}
;;           | <id>
;;           | {fun {<id>} <RCFAE>}
;;           | {<RCFAE> <RCFAE>}
;;           | {if0 {RCFAE} {RCFAE} {RCFAE}
;;           | {rec {<id> <RCFAE>} <RCFAE>}

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (c RCFAE?) (t-branch RCFAE?) (f-branch RCFAE?)]
  [recu (name symbol?) (named-expr RCFAE?) (body RCFAE?)])

(define-type RCFAE-Value
  [noneV]
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (env Env?)])

(define-type Env
  [emptyEnv]
  [consEnv (name symbol?) (value RCFAE-Value?) (next Env?)]
  [consRecEnv (name symbol?) (value boxed-RCFAE-Value?) (next Env?)])

;; boxed-RCFAE-Value? : any -> boolean
(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

(test (boxed-RCFAE-Value? (numV 1)) #f)
(test (boxed-RCFAE-Value? (box 1)) #f)
(test (boxed-RCFAE-Value? (box (numV 1))) #t)
(test (boxed-RCFAE-Value? (box (closureV 'x (add (id 'x) (id 'x)) (emptyEnv)))) #t)

;; parse : sexp -> RCFAE
(define (parse sexp)
  (cond [(number? sexp) (num sexp)]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) '+))
         (add (parse (second sexp))
              (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) '*))
         (mul (parse (second sexp))
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
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'rec)
              (list? (second sexp)) (= (length (second sexp)) 2) (symbol? (first (second sexp))))
         (recu (first (second sexp))
               (parse (second (second sexp)))
               (parse (third sexp)))]))

(test (parse '{with {x {+ 3 4}} {* x x}})
      (app (fun 'x (mul (id 'x) (id 'x))) (add (num 3) (num 4))))
(test (parse '{{fun {x} {+ x {* x x}}} {if0 0 {+ 3 4} {* 3 4}}})
      (app (fun 'x (add (id 'x) (mul (id 'x) (id 'x))))
           (if0 (num 0) (add (num 3) (num 4)) (mul (num 3) (num 4)))))
(test (parse '{rec {fac {fun {n}
                             {if0 n
                                  1
                                  {* n {fac {+ n -1}}}}}}
                {fac 5}})
      (recu 'fac
            (fun 'n (if0 (id 'n)
                         (num 1)
                         (mul (id 'n)
                              (app (id 'fac) (add (id 'n) (num -1))))))
            (app (id 'fac) (num 5))))

;; lookup : symbol Env -> RCFAE-Value
(define (lookup name env)
  (type-case Env env
    [emptyEnv () (error 'lookup "no bound identifier")]
    [consEnv (n v next) (if (symbol=? name n)
                            v
                            (lookup name next))]
    [consRecEnv (n v next) (if (symbol=? name n)
                               (if (noneV? (unbox v))
                                   (error 'lookup "there is no value here yet")
                                   (unbox v))
                               (lookup name next))]))

(test (lookup 'x (consEnv 'x
                          (numV 3)
                          (consEnv 'y
                                   (closureV 'x
                                             (add (id 'x) (id 'y))
                                             (consEnv 'y (numV 1) (emptyEnv)))
                                   (emptyEnv))))
      (numV 3))

;; cyclically-bind-and-interp : symbol RCFAE Env -> Env
(define (cyclically-bind-and-interp fun-name fun-def env)
  (local [(define value-holder (box (noneV)))
          (define new-env (consRecEnv fun-name value-holder env))
          (define fun-val (interp fun-def new-env))]
    (begin (set-box! value-holder fun-val)
           new-env)))

;; num+ : RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(test (num+ (numV 1) (numV 2)) (numV 3))

;; num* : RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num* n1 n2)
  (numV (* (numV-n n1) (numV-n n2))))

(test (num* (numV 3) (numV 4)) (numV 12))

;; zero-num? : RCFAE-Value -> RCFAE-Value
(define (zero-num? n)
  (zero? (numV-n n)))

;; interp : RCFAE -> RCFAE-Value
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env)
                     (interp r env))]
    [mul (l r) (num* (interp l env)
                     (interp r env))]
    [id (v) (lookup v env)]
    [fun (param body) (closureV param body env)]
    [app (fun-expr arg-expr) (local [(define the-fun (interp fun-expr env))]
                               (interp (closureV-body the-fun)
                                       (consEnv (closureV-param the-fun)
                                                (interp arg-expr env)
                                                env)))]
    [if0 (c t-branch f-branch) (local [(define value (interp c env))]
                                 (if (zero-num? value)
                                     (interp t-branch env)
                                     (interp f-branch env)))]
    [recu (name named-expr body) (interp body
                                         (cyclically-bind-and-interp name
                                                                     named-expr
                                                                     env))]))

(test (interp (parse '{with {double {fun {x} {+ x x}}}
                            {with {sqr {fun {x} {* x x}}}
                                  {with {y 3}
                                        {if0 y {double y} {sqr y}}}}})
              (emptyEnv))
      (numV 9))
(test (interp (parse '{rec {fac {fun {x} {if0 x 1 {* x {fac {+ x -1}}}}}} {fac 6}})
              (emptyEnv))
      (numV 720))
