#lang plai

;; <RVCFAE> ::= <num>
;;            | {+ <RVCFAE> <RVCFAE>}
;;            | {with {<id> <RVCFAE>} <RVCFAE>}
;;            | <id>
;;            | {fun {<id>} <RVCFAE>}
;;            | {<RVCFAE> <RVCFAE>}
;;            | {if0 <RVCFAE> <RVCFAE> <RVCFAE>}
;;            | {set <id> <RVCFAE>}
;;            | {seqn <RVCFAE> <RVCFAE>}
;;            | {ref <id>}

(define-type RVCFAE
  [num (n number?)]
  [add (lhs RVCFAE?) (rhs RVCFAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body RVCFAE?)]
  [app (fun RVCFAE?) (arg RVCFAE?)]
  [if0 (test RVCFAE?) (truth RVCFAE?) (falsity RVCFAE?)]
  [setv (var symbol?) (val RVCFAE?)]
  [seqn (e1 RVCFAE?) (e2 RVCFAE?)]
  [ref (var symbol?)])

(define-type RVCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body RVCFAE?) (env Env?)]
  [refV (var symbol?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (loc number?) (rest Env?)])

(define-type Store
  [mtSto]
  [aSto (loc number?) (value RVCFAE-Value?) (rest Store?)])

(define-type Value*Store
  [v*s (value RVCFAE-Value?) (store Store?)])

;; num+ : RVCFAE-Value RVCFAE-Value -> RVCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

;; num-zero? : RVCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

;; env-lookup : symbol Env -> number
(define (env-lookup v env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding identifier")]
    [aSub (name loc rest)
          (if (symbol=? v name)
              loc
              (env-lookup v rest))]))

;; store-lookup : number Store -> RCFAE-Value
(define (store-lookup l store)
  (type-case Store store
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (loc value rest)
          (if (= l loc)
              value
              (store-lookup l rest))]))

;; next-location : -> number
(define next-location
  (local [(define last-loc (box -1))]
    (lambda ()
      (begin (set-box! last-loc (+ (unbox last-loc) 1))
             (unbox last-loc)))))

;; parse : sexp -> RVCFAE
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
        [(and (list? sexp) (= (length sexp) 4) (eq? (first sexp) 'if0))
         (if0 (parse (second sexp))
              (parse (third sexp))
              (parse (fourth sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'set) (symbol? (second sexp)))
         (setv (second sexp)
               (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'seqn))
         (seqn (parse (second sexp))
               (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 2) (eq? (first sexp) 'ref) (symbol? (second sexp)))
         (ref (second sexp))]
        [(and (list? sexp) (= (length sexp) 2))
         (app (parse (first sexp))
              (parse (second sexp)))]))

(test (parse '{with {v 0}
                    {with {f {fun {y}
                                  {set y 5}}}
                          {with {g {fun {y}
                                        {set y 5}}}
                                {seqn {if0 v {f v} {g v}}
                                      v}}}})
      (app (fun 'v (app (fun 'f (app (fun 'g (seqn (if0 (id 'v)
                                                        (app (id 'f) (id 'v))
                                                        (app (id 'g) (id 'v)))
                                                   (id 'v)))
                                     (fun 'y (setv 'y (num 5)))))
                        (fun 'y (setv 'y (num 5)))))
           (num 0)))
(test (parse '{with {v 0}
                    {with {f {fun {x} {set x 5}}}
                          {f {ref v}}}})
      (app (fun 'v (app (fun 'f (app (id 'f) (ref 'v)))
                        (fun 'x (setv 'x (num 5)))))
           (num 0)))

;; interp : RVCFAE Env Store -> RVCFAE-Value
(define (interp expr env store)
  (type-case RVCFAE expr
    [num (n) (v*s (numV n) store)]
    [add (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (num+ l-value r-value) r-store)])])]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [fun (param body) (v*s (closureV param body env) store)]
    [app (fun-e arg-e)
         (type-case Value*Store (interp fun-e env store)
           [v*s (fun-value fun-store)
                (type-case Value*Store (interp arg-e env fun-store)
                  [v*s (arg-value arg-store)
                       (if (refV? arg-value)
                           (interp (closureV-body fun-value)
                                   (aSub (closureV-param fun-value)
                                         (env-lookup (refV-var arg-value) env)
                                         (closureV-env fun-value))
                                   store)
                           (local [(define new-loc (next-location))]
                             (interp (closureV-body fun-value)
                                     (aSub (closureV-param fun-value)
                                           new-loc
                                           (closureV-env fun-value))
                                     (aSto new-loc arg-value arg-store))))])])]
    [if0 (test truth falsity)
         (type-case Value*Store (interp test env store)
           [v*s (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [setv (var val)
          (type-case Value*Store (interp val env store)
            [v*s (val-value val-store)
                 (v*s val-value
                      (aSto (env-lookup var env)
                            val-value
                            val-store))])]
    [seqn (e1 e2)
          (type-case Value*Store (interp e1 env store)
            [v*s (e1-value e1-store) (interp e2 env e1-store)])]
    [ref (v) (v*s (refV v) store)]))

(test (v*s-value (interp (parse '{with {d 1}
                                       {with {add-d {fun {x} {+ x d}}}
                                             {with {double {fun {x} {+ x x}}}
                                                   {with {x 5}
                                                         {if0 x {add-d x} {double x}}}}}})
                         (mtSub)
                         (mtSto)))
      (numV 10))
(test (v*s-value (interp (parse '{with {v 0}
                                       {with {f {fun {y}
                                                     {set y 5}}}
                                             {seqn {f v}
                                                   v}}})
                         (mtSub)
                         (mtSto)))
      (numV 0))
(test (v*s-value (interp (parse '{with {swap {fun {x}
                                                  {fun {y}
                                                       {with {z x}
                                                             {seqn {set x y}
                                                                   {set y z}}}}}}
                                       {with {a 3}
                                             {with {b 2}
                                                   {seqn {{swap a} b}
                                                         b}}}})
                         (mtSub)
                         (mtSto)))
      (numV 2))
(test (v*s-value (interp (parse '{with {swap {fun {x}
                                                  {fun {y}
                                                       {with {z x}
                                                             {seqn {set x y}
                                                                   {set y z}}}}}}
                                       {with {a 3}
                                             {with {b 2}
                                                   {seqn {{swap {ref a}} {ref b}}
                                                         b}}}})
                         (mtSub)
                         (mtSto)))
      (numV 3))
