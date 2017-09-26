#lang plai

;; <FAE> ::= <num>
;;         | {+ <FAE> <FAE>}
;;         | {with {<id> <FAE>} <FAE>}
;;         | <id>
;;         | {fun {<id>} <FAE>}
;;         | {<FAE> <FAE>}

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (env Env?)])

;; Env? : any -> boolean
(define (Env? x)
  (procedure? x))

;; aSub : symbol Value Env -> Env
(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (if (symbol=? want-name bound-name)
        bound-value
        (lookup want-name env))))

;; mtSub : symbol -> Env
(define (mtSub)
  (lambda (want-name)
    (error 'lookup "no binding for identifier")))

;; lookup : symbol Env
(define (lookup name env)
  (env name))

;; parse : sexp -> FAE
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
              (parse (second sexp)))]))

(test (parse '{with {x 1} {+ x x}}) (app (fun 'x (add (id 'x) (id 'x))) (num 1)))
(test (parse '{{fun {x} {+ x x}} 1}) (app (fun 'x (add (id 'x) (id 'x))) (num 1)))

;; interp : FAE Env -> FAE-Value
(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env)
                     (interp r env))]
    [id (v) (lookup v env)]
    [fun (param body) (closureV param body env)]
    [app (fun-expr arg-expr) (local [(define the-fun (interp fun-expr env))]
                               (interp (closureV-body the-fun)
                                       (aSub (closureV-param the-fun)
                                             (interp arg-expr env)
                                             env)))]))

;; num+ : FAE-Value FAE-Value -> FAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

(test (num+ (numV 1) (numV 2)) (numV 3))

(test (interp (parse '{with {x 1} {+ x x}}) (mtSub)) (numV 2))
(test (interp (parse '{{fun {x} {+ x x}} 1}) (mtSub)) (numV 2))
(test (interp (parse '{with {d 1} {with {add-d {fun {x} {+ x d}}} {add-d 5}}})
                (mtSub))
      (numV 6))
