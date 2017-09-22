#lang plai

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (v symbol?)]
  [fun (arg-name symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)]
  [if0 (condition CFAE/L?) (true-branch CFAE/L?) (false-branch CFAE/L?)])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (arg-name symbol?) (body CFAE/L?) (env Env?)]
  [exprV (expr CFAE/L?) (env Env?) (cache boxed-boolean/CFAE/L-Value?)])

(define-type Env
  [emptyEnv]
  [consEnv (name symbol?) (expr CFAE/L-Value?) (rest-env Env?)])

;; boxed-boolean/CFAE/L-Value? -> boolean
(define (boxed-boolean/CFAE/L-Value? v)
  (and (box? v)
       (or (boolean? (unbox v))
           (numV? (unbox v))
           (closureV? (unbox v)))))

;; num-sexp? : sexp -> boolean
(define (num-sexp? sexp)
  (number? sexp))

;; add-sexp? : sexp -> boolean
(define (add-sexp? sexp)
  (and (list? sexp) (= (length sexp) 3) (eq? (first sexp) '+)))

;; with-sexp? : sexp -> boolean
(define (with-sexp? sexp)
  (and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'with)
       (list? (second sexp)) (= (length (second sexp)) 2) (symbol? (first (second sexp)))))

;; id-sexp? : sexp -> boolean
(define (id-sexp? sexp)
  (symbol? sexp))

;; fun-sexp? : sexp -> boolean
(define (fun-sexp? sexp)
  (and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'fun)
       (list? (second sexp)) (= (length (second sexp)) 1) (symbol? (first (second sexp)))))

;; app-sexp? : sexp -> boolean
(define (app-sexp? sexp)
  (and (list? sexp) (= (length sexp) 2)))

;; if0-sexp? : sexp -> boolean
(define (if0-sexp? sexp)
  (and (list? sexp) (= (length sexp) 4) (eq? (first sexp) 'if0)))

;; pre-process : sexp -> sexp
;; convert with expressions to function applications
(define (pre-process sexp)
  (cond [(num-sexp? sexp) sexp]
        [(add-sexp? sexp) (list '+
                                (pre-process (second sexp))
                                (pre-process (third sexp)))]
        [(with-sexp? sexp) (list (list 'fun
                                       (list (first (second sexp)))
                                       (pre-process (third sexp)))
                                 (pre-process (second (second sexp))))]
        [(id-sexp? sexp) sexp]
        [(fun-sexp? sexp) (list 'fun
                                (list (first (second sexp)))
                                (pre-process (third sexp)))]
        [(app-sexp? sexp) (list (pre-process (first sexp))
                                (pre-process (second sexp)))]
        [(if0-sexp? sexp) (list 'if0
                                (pre-process (second sexp))
                                (pre-process (third sexp))
                                (pre-process (fourth sexp)))]))

(test (pre-process '{with {x 1} {+ x x}})
      '{{fun {x} {+ x x}} 1})
(test (pre-process '{with {double {fun {x} {+ x x}}}
                          {with {y 9}
                                {double y}}})
      '{{fun {double}
             {{fun {y}
                   {double y}}
              9}}
        {fun {x} {+ x x}}})
(test (pre-process '{with {x 1} {with {y 3} {if0 x {+ y y} {+ y {+ y y}}}}})
      '{{fun {x}
             {{fun {y}
                   {if0 x {+ y y} {+ y {+ y y}}}}
              3}}
        1})

;; parse : sexp -> CFAE/L
(define (parse sexp)
  (cond [(num-sexp? sexp) (num sexp)]
        [(add-sexp? sexp) (add (parse (second sexp))
                               (parse (third sexp)))]
        [(id-sexp? sexp) (id sexp)]
        [(fun-sexp? sexp) (fun (first (second sexp))
                               (parse (third sexp)))]
        [(app-sexp? sexp) (app (parse (first sexp))
                               (parse (second sexp)))]
        [(if0-sexp? sexp) (if0 (parse (second sexp))
                               (parse (third sexp))
                               (parse (fourth sexp)))]))

(test (parse '{{fun {x} {+ x x}} 1})
      (app (fun 'x (add (id 'x) (id 'x)))
           (num 1)))
(test (parse (pre-process '{with {x 1} {with {y 3} {if0 x {+ y y} {+ y {+ y y}}}}}))
      (app (fun 'x
                (app (fun 'y
                          (if0 (id 'x)
                               (add (id 'y) (id 'y))
                               (add (id 'y) (add (id 'y) (id 'y)))))
                     (num 3)))
           (num 1)))

;; lookup : symbol Env -> CFAE/L-Value
(define (lookup name env)
  (cond [(emptyEnv? env) (error 'lookup "unbound identifer")]
        [(symbol=? name (consEnv-name env)) (consEnv-expr env)]
        [else (lookup name (consEnv-rest-env env))]))

(test (lookup 'y (consEnv 'x
                          (exprV (num 3) (emptyEnv) (box false))
                          (consEnv 'y
                                   (exprV (fun 'x (add (id 'x) (id 'delta)))
                                          (consEnv 'delta (exprV (num 1) (emptyEnv) (box false)) (emptyEnv))
                                          (box false))
                                   (emptyEnv))))
      (exprV (fun 'x (add (id 'x) (id 'delta)))
             (consEnv 'delta (exprV (num 1) (emptyEnv) (box false)) (emptyEnv))
             (box false)))

(define (interp expr)
  ;; strict : CFAE/L-Value -> CFAE/L-Value [excluding exprV]
  (define (strict value)
    (type-case CFAE/L-Value value
      [exprV (expr env cache) (if (boolean? (unbox cache))
                                  (local [(define the-value (strict (interp expr env)))]
                                    (begin (printf "Forcnig exprV ~a to ~a~n" expr the-value)
                                           (set-box! cache the-value)
                                           the-value))
                                  (begin (printf "Using cached value~n")
                                         (unbox cache)))]
      [else value]))

  ;; num+ : CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
  (define (num+ n1 n2)
    (numV (+ (numV-n (strict n1))
             (numV-n (strict n2)))))

  ;; num-zero? : CFAE/L-Value -> boolean
  (define (num-zero? n)
    (zero? (numV-n (strict n))))

  ;; interp : CFAE/L Env -> CFAE/L-Value
  (define (interp expr env)
    (type-case CFAE/L expr
      [num (n) (numV n)]
      [add (l r) (num+ (interp l env)
                       (interp r env))]
      [id (v) (lookup v env)]
      [fun (arg-name body) (closureV arg-name body env)]
      [app (fun-expr arg-expr) (local [(define the-fun (strict (interp fun-expr env)))]
                                 (interp (closureV-body the-fun)
                                         (consEnv (closureV-arg-name the-fun)
                                                  (exprV arg-expr env (box false))
                                                  (closureV-env the-fun))))]
      [if0 (condition true-branch false-branch) (if (num-zero? (interp condition env))
                                                    (interp true-branch env)
                                                    (interp false-branch env))]))
  (strict (interp expr (emptyEnv))))

(test (interp (parse (pre-process '{with {x 3} x}))) (numV 3))
(test (interp (parse (pre-process '{with {x {+ 4 5}}
                                         {with {y {+ x x}}
                                               {with {z y}
                                                     {with {x 4}
                                                           z}}}})))
      (numV 18))
(test (interp (parse (pre-process '{with {y {undef x}} 4}))) (numV 4))
(test (interp (parse (pre-process '{with {x 0} {if0 x {+ 1 2} {undef y}}}))) (numV 3))
