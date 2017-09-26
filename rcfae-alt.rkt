#lang plai

;; <RCFAE> ::= <num>
;;           | {+ <RCFAE> <RCFAE>}
;;           | {* <RCFAE> <RCFAE>}
;;           | {with {<id> <RCFAE>} <RCFAE>}
;;           | <id>
;;           | {fun {<id>} <RCFAE>}
;;           | {<RCFAE> <RCFAE>}
;;           | {if0 <RCFAE> <RCFAE> <RCFAE>}
;;           | {rec {<id> <RCFAE>} <RCFAE>}

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [mul (lhs RCFAE?) (rhs RCFAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (condition RCFAE?) (t-branch RCFAE?) (f-branch RCFAE?)]
  [recu (name symbol?) (named-expr RCFAE?) (body RCFAE?)])

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RCFAE?) (env Env?)])

;; Env? : any -> boolean
(define (Env? x)
  (procedure? x))

;; aSub : symbol RCFAE-Value Env -> Env
(define (aSub name value env)
  (lambda (want-value)
    (if (symbol=? want-value name)
        value
        (lookup want-value env))))

;; mtSub : -> Env
(define (mtSub)
  (lambda (want-value)
    (error 'lookup "no binding identifier")))

;; lookup : symbol Env -> RCFAE-Value
(define (lookup name env)
  (env name))

(test (lookup 'x (aSub 'x (numV 1) (aSub 'y (closureV 'x (add (id 'x) (id 'x)) (mtSub)) (mtSub))))
      (numV 1))

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

(test (parse '{with {d 1} {{fun {x} {+ x d}} 2}})
      (app (fun 'd
                (app (fun 'x (add (id 'x) (id 'd)))
                     (num 2)))
           (num 1)))
(test (parse '{rec {fac {fun {x} {if0 x 1 {* x {fac {+ x -1}}}}}} {fac 5}})
      (recu 'fac
            (fun 'x (if0 (id 'x)
                         (num 1)
                         (mul (id 'x) (app (id 'fac) (add (id 'x) (num -1))))))
            (app (id 'fac) (num 5))))

;; interp : RCFAE Env -> RCFAE-Value
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
                                       (aSub (closureV-param the-fun)
                                             (interp arg-expr env)
                                             (closureV-env the-fun))))]
    [if0 (c t-branch f-branch) (if (num-zero? (interp c env))
                                   (interp t-branch env)
                                   (interp f-branch env))]
    [recu (name named-expr body) (interp body
                                         (cyclically-bind-and-interp name
                                                                     named-expr
                                                                     env))]))

;; num+ : RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

;; num* : RCFAE-Value RCFAE-Value -> RCFAE-Value
(define (num* n1 n2)
  (numV (* (numV-n n1)
           (numV-n n2))))

;; num-zero? : RCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; cyclically-bind-and-interp : symbol RCFAE Env -> Env
(define (cyclically-bind-and-interp name named-expr env)
  (local [(define rec-ext-env
            (lambda (want-value)
              (if (symbol=? want-value name)
                  (closureV (fun-param named-expr)
                            (fun-body named-expr)
                            rec-ext-env)
                  (lookup want-value env))))]
    rec-ext-env))

(test (interp (parse '{with {d 1} {{fun {x} {+ x d}} 2}}) (mtSub)) (numV 3))
(test (interp (parse '{rec {fac {fun {x} {if0 x 1 {* x {fac {+ x -1}}}}}} {fac 5}}) (mtSub)) (numV 120))
