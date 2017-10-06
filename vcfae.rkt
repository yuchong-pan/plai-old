#lang plai

;; <VCFAE> ::= <num>
;;           | {+ <VCFAE> <VCFAE>}
;;           | {with {<id> <VCFAE>} <VCFAE>}
;;           | <id>
;;           | {fun {<id>} <VCFAE>}
;;           | {<VCFAE> <VCFAE>}
;;           | {if0 <VCFAE> <VCFAE> <VCFAE>}
;;           | {set <id> <VCFAE>}
;;           | {seqn <VCFAE> <VCFAE>}

(define-type VCFAE
  [num (n number?)]
  [add (lhs VCFAE?) (rhs VCFAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body VCFAE?)]
  [app (fun VCFAE?) (arg VCFAE?)]
  [if0 (test VCFAE?) (truth VCFAE?) (falsity VCFAE?)]
  [setv (id symbol?) (val VCFAE?)]
  [seqn (e1 VCFAE?) (e2 VCFAE?)])

(define-type VCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body VCFAE?) (env Env?)])

(define-type Env
  [aSub (name symbol?) (loc number?) (env Env?)]
  [mtSub])

(define-type Store
  [aSto (loc number?) (value VCFAE-Value?) (store Store?)]
  [mtSto])

(define-type Value*Store
  [v*s (value VCFAE-Value?) (store Store?)])

;; env-lookup : symbol Env -> number
(define (env-lookup want env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (name loc rest)
          (if (symbol=? want name)
              loc
              (env-lookup want rest))]))

;; store-lookup : number Store -> VCFAE-Value
(define (store-lookup want store)
  (type-case Store store
    [mtSto () (error 'lookup "no value at location")]
    [aSto (loc value rest)
          (if (= want loc)
              value
              (store-lookup want rest))]))

;; next-location : -> number
(define next-location
  (local [(define last-loc (box -1))]
    (lambda ()
      (begin (set-box! last-loc (+ (unbox last-loc) 1))
             (unbox last-loc)))))

;; parse : sexp -> VCFAE
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
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'set) (symbol? (second sexp)))
         (setv (second sexp)
               (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'seqn))
         (seqn (parse (second sexp))
               (parse (third sexp)))]))

(test (parse '{with {d 2}
                    {{fun {x}
                          {if0 x {+ x 233} {+ x d}}}
                     5}})
      (app (fun 'd (app (fun 'x (if0 (id 'x)
                                     (add (id 'x) (num 233))
                                     (add (id 'x) (id 'd))))
                        (num 5)))
           (num 2)))
(test (parse '{seqn {set x 5}
                    {with {d 2} {+ d d}}})
      (seqn (setv 'x (num 5))
            (app (fun 'd (add (id 'd) (id 'd))) (num 2))))

;; interp : VCFAE Env Store -> Value*Store
(define (interp expr env store)
  (type-case VCFAE expr
    [num (n) (v*s (numV n) store)]
    [add (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (num+ l-value r-value)
                            r-store)])])]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [fun (param body) (v*s (closureV param body env) store)]
    [app (fun-e arg-e)
         (type-case Value*Store (interp fun-e env store)
           [v*s (fun-value fun-store)
                (type-case Value*Store (interp arg-e env store)
                  [v*s (arg-value arg-store)
                       (local [(define new-loc (next-location))]
                         (interp (closureV-body fun-value)
                                 (aSub (closureV-param fun-value)
                                       new-loc
                                       (closureV-env fun-value))
                                 (aSto new-loc arg-value arg-store)))])])]
    [if0 (test truth falsity)
         (type-case Value*Store (interp test env store)
           [v*s (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [setv (id value)
          (type-case Value*Store (interp value env store)
            [v*s (val-value val-store)
                 (local [(define loc (env-lookup id env))]
                   (v*s val-value
                        (aSto loc val-value val-store)))])]
    [seqn (e1 e2)
          (type-case Value*Store (interp e1 env store)
            [v*s (e1-value e1-store)
                 (interp e2 env e1-store)])]))

;; num+ : VCFAE-Value VCFAE-Value -> VCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1)
           (numV-n n2))))

;; num-zero? : VCFAE-Value -> boolean
(define (num-zero? n)
  (and (numV? n) (zero? (numV-n n))))

(test (v*s-value (interp (parse '{with {v 0}
                                       {with {f {fun {y}
                                                     {set y 5}}}
                                             {seqn {f v}
                                                   v}}})
                         (mtSub)
                         (mtSto)))
      (numV 0))
