#lang plai

(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (v symbol?)]
  [fun (fun-name (or/c false? symbol?)) (arg-name symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)]
  [if0 (condition CFAE/L?) (true-branch CFAE/L?) (false-branch CFAE/L?)]
  [lst (head CFAE/L?) (tail CFAE/L?)]
  [hd (lst CFAE/L?)]
  [tl (lst CFAE/L?)]
  [emptyLst])

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (fun-name (or/c false? symbol?)) (arg-name symbol?) (body CFAE/L?) (env Env?)]
  [exprV (expr CFAE/L?) (env Env?)]
  [lstV (head CFAE/L-Value?) (tail CFAE/L-Value?)]
  [emptyLstV])

(define-type Env
  [emptyEnv]
  [consEnv (name symbol?) (expr CFAE/L-Value?) (rest-env Env?)])

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
  (and (list? sexp) (= (length sexp) 4) (eq? (first sexp) 'fun)
       (or (false? (second sexp)) (symbol? (second sexp)))
       (list? (third sexp)) (= (length (third sexp)) 1) (symbol? (first (third sexp)))))

;; app-sexp? : sexp -> boolean
(define (app-sexp? sexp)
  (and (list? sexp) (= (length sexp) 2)
       (not (or (eq? (first sexp) 'hd) (eq? (first sexp) 'tl)))))

;; if0-sexp? : sexp -> boolean
(define (if0-sexp? sexp)
  (and (list? sexp) (= (length sexp) 4) (eq? (first sexp) 'if0)))

;; lst-sexp? : sexp -> boolean
(define (lst-sexp? sexp)
  (and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'lst)))

;; hd-sexp? : sexp -> boolean
(define (hd-sexp? sexp)
  (and (list? sexp) (= (length sexp) 2) (eq? (first sexp) 'hd)))

;; tl-sexp? : sexp -> boolean
(define (tl-sexp? sexp)
  (and (list? sexp) (= (length sexp) 2) (eq? (first sexp) 'tl)))

;; emptyLst-sexp? : sexp -> boolean
(define (emptyLst-sexp? sexp)
  (and (list? sexp) (= (length sexp) 0)))

;; pre-process : sexp -> sexp
;; convert with expressions to function applications
(define (pre-process sexp)
  (cond [(num-sexp? sexp) sexp]
        [(add-sexp? sexp) (list '+
                                (pre-process (second sexp))
                                (pre-process (third sexp)))]
        [(with-sexp? sexp) (list (list 'fun
                                       #f
                                       (list (first (second sexp)))
                                       (pre-process (third sexp)))
                                 (pre-process (second (second sexp))))]
        [(id-sexp? sexp) sexp]
        [(fun-sexp? sexp) (list 'fun
                                (second sexp)
                                (list (first (third sexp)))
                                (pre-process (fourth sexp)))]
        [(hd-sexp? sexp) (list 'hd (pre-process (second sexp)))]
        [(tl-sexp? sexp) (list 'tl (pre-process (second sexp)))]
        [(app-sexp? sexp) (list (pre-process (first sexp))
                                (pre-process (second sexp)))]
        [(if0-sexp? sexp) (list 'if0
                                (pre-process (second sexp))
                                (pre-process (third sexp))
                                (pre-process (fourth sexp)))]
        [(lst-sexp? sexp) (list 'lst
                                (pre-process (second sexp))
                                (pre-process (third sexp)))]
        [(emptyLst-sexp? sexp) sexp]))

(test (pre-process '{with {x 1} {+ x x}})
      '{{fun #f {x} {+ x x}} 1})
(test (pre-process '{with {double {fun double {x} {+ x x}}}
                          {with {y 9}
                                {double y}}})
      '{{fun #f
             {double}
             {{fun #f {y} {double y}} 9}}
        {fun double {x} {+ x x}}})
(test (pre-process '{with {x 1} {with {y 3} {if0 x {+ y y} {+ y {+ y y}}}}})
      '{{fun #f
             {x}
             {{fun #f {y} {if0 x {+ y y} {+ y {+ y y}}}}
              3}}
        1})

;; parse : sexp -> CFAE/L
(define (parse sexp)
  (cond [(num-sexp? sexp) (num sexp)]
        [(add-sexp? sexp) (add (parse (second sexp))
                               (parse (third sexp)))]
        [(id-sexp? sexp) (id sexp)]
        [(fun-sexp? sexp) (fun (second sexp)
                               (first (third sexp))
                               (parse (fourth sexp)))]
        [(hd-sexp? sexp) (hd (parse (second sexp)))]
        [(tl-sexp? sexp) (tl (parse (second sexp)))]
        [(app-sexp? sexp) (app (parse (first sexp))
                               (parse (second sexp)))]
        [(if0-sexp? sexp) (if0 (parse (second sexp))
                               (parse (third sexp))
                               (parse (fourth sexp)))]
        [(lst-sexp? sexp) (lst (parse (second sexp))
                               (parse (third sexp)))]
        [(emptyLst-sexp? sexp) (emptyLst)]))

(test (parse '{{fun #f {x} {+ x x}} 1})
      (app (fun #f 'x (add (id 'x) (id 'x)))
           (num 1)))
(test (parse (pre-process '{with {x 1} {with {y 3} {if0 x {+ y y} {+ y {+ y y}}}}}))
      (app (fun #f
                'x
                (app (fun #f
                          'y
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
                          (exprV (num 3) (emptyEnv))
                          (consEnv 'y
                                   (exprV (fun 'add-delta 'x (add (id 'x) (id 'delta)))
                                          (consEnv 'delta (exprV (num 1) (emptyEnv)) (emptyEnv)))
                                   (emptyEnv))))
      (exprV (fun 'add-delta 'x (add (id 'x) (id 'delta)))
             (consEnv 'delta (exprV (num 1) (emptyEnv)) (emptyEnv))))

(define (interp expr)
  ;; strict : CFAE/L-Value -> CFAE/L-Value [excluding exprV]
  (define (strict value)
    (type-case CFAE/L-Value value
      [exprV (expr env) (strict (interp expr env))]
      [else value]))

  ;; num+ : CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
  (define (num+ n1 n2)
    (numV (+ (numV-n (strict n1))
             (numV-n (strict n2)))))

  ;; num-zero? : CFAE/L-Value -> boolean
  (define (num-zero? n)
    (zero? (numV-n (strict n))))

  ;; cons-lst : CFAE/L-Value CFAE/L-Value -> CFAE/L-Value
  (define (cons-lst head tail)
    (lstV (strict head)
          (strict tail)))

  ;; interp : CFAE/L Env -> CFAE/L-Value
  (define (interp expr env)
    (type-case CFAE/L expr
      [num (n) (numV n)]
      [add (l r) (num+ (interp l env)
                       (interp r env))]
      [id (v) (lookup v env)]
      [fun (fun-name arg-name body) (closureV fun-name arg-name body env)]
      [app (fun-expr arg-expr) (local [(define the-fun (strict (interp fun-expr env)))]
                                 (interp (closureV-body the-fun)
                                         (consEnv (closureV-arg-name the-fun)
                                                  (exprV arg-expr env)
                                                  (if (false? (closureV-fun-name the-fun))
                                                      (closureV-env the-fun)
                                                      (consEnv (closureV-fun-name the-fun)
                                                               the-fun
                                                               (closureV-env the-fun))))))]
      [if0 (condition true-branch false-branch) (if (num-zero? (interp condition env))
                                                    (interp true-branch env)
                                                    (interp false-branch env))]
      [lst (head tail) (cons-lst (interp head env)
                                 (interp tail env))]
      [hd (lst) (strict (lstV-head (strict (interp lst env))))]
      [tl (lst) (strict (lstV-tail (strict (interp lst env))))]
      [emptyLst () (emptyLstV)]))
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
(test (interp (parse (pre-process '{}))) (emptyLstV))
(test (interp (parse (pre-process '{lst 1 {lst 2 {}}}))) (lstV (numV 1) (lstV (numV 2) (emptyLstV))))
(test (interp (parse (pre-process '{lst 1 {lst {fun double {x} {+ x x}} {}}})))
      (lstV (numV 1) (lstV (closureV 'double 'x (add (id 'x) (id 'x)) (emptyEnv)) (emptyLstV))))
(test (interp (parse (pre-process '{hd {lst 1 {lst 2 {lst 3 {}}}}}))) (numV 1))
(test (interp (parse (pre-process '{tl {lst 1 {lst 2 {lst 3 {}}}}}))) (lstV (numV 2) (lstV (numV 3) (emptyLstV))))
(test (interp (parse (pre-process '{{fun sum
                                         {x}
                                         {if0 x
                                              0
                                              {+ x {sum {+ x -1}}}}}
                                    5})))
      (numV 15))
(test (interp (parse (pre-process '{{fun fib
                                         {n}
                                         {if0 n
                                              0
                                              {if0 {+ n -1}
                                                   1
                                                   {+ {fib {+ n -1}}
                                                      {fib {+ n -2}}}}}}
                                    10})))
      (numV 55))
