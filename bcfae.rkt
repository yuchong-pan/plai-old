#lang plai

;; <BCFAE> ::= <num>
;;           | {+ <BCFAE> <BCFAE>}
;;           | {with {<id> <BCFAE>} <BCFAE>}
;;           | <id>
;;           | {fun {x} <BCFAE>}
;;           | {<BCFAE> <BCFAE>}
;;           | {if0 <BCFAE> <BCFAE> <BCFAE>}
;;           | {newbox <BCFAE>}
;;           | {setbox <BCFAE> <BCFAE>}
;;           | {openbox <BCFAE>}
;;           | {seqn <BCFAE> ...}

(define-type BCFAE
  [num (n number?)]
  [add (lhs BCFAE?) (rhs BCFAE?)]
  [id (v symbol?)]
  [fun (param symbol?) (body BCFAE?)]
  [app (fun-expr BCFAE?) (arg-expr BCFAE?)]
  [if0 (test BCFAE?) (truth BCFAE?) (falsity BCFAE?)]
  [newbox (val-expr BCFAE?)]
  [setbox (box-expr BCFAE?) (val-expr BCFAE?)]
  [openbox (box-expr BCFAE?)]
  [seqn (fst BCFAE?) (rst seqn-type?)]
  [mtSeqn])

(define-type BCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body BCFAE?) (env Env?)]
  [boxV (loc number?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?) (location number?) (env Env?)])

(define-type Store
  [mtSto]
  [aSto (location number?) (value BCFAE-Value?) (store Store?)])

(define-type Value*Store
  [v*s (value BCFAE-Value?) (store Store?)])

;; seqn-type? : BCFAE -> boolean
(define (seqn-type? expr)
  (or (seqn? expr) (mtSeqn? expr)))

;; parse : sexp -> BCFAE
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
        [(and (list? sexp) (= (length sexp) 2) (eq? (first sexp) 'newbox))
         (newbox (parse (second sexp)))]
        [(and (list? sexp) (= (length sexp) 3) (eq? (first sexp) 'setbox))
         (setbox (parse (second sexp))
                 (parse (third sexp)))]
        [(and (list? sexp) (= (length sexp) 2) (eq? (first sexp) 'openbox))
         (openbox (parse (second sexp)))]
        [(and (list? sexp) (> (length sexp) 1) (eq? (first sexp) 'seqn))
         (parse-seqn (rest sexp))]
        [(and (list? sexp) (= (length sexp) 2))
         (app (parse (first sexp))
              (parse (second sexp)))]))

;; parse-seqn : sexp -> BCFAE
(define (parse-seqn sexp)
  (cond [(empty? sexp) (mtSeqn)]
        [else (seqn (parse (first sexp))
                    (parse-seqn (rest sexp)))]))

(test (parse '{with {d 1} {{fun {x} {+ x d}} 2}})
      (app (fun 'd (app (fun 'x (add (id 'x) (id 'd))) (num 2))) (num 1)))
(test (parse '{with {a {newbox 1}}
                    {with {f {fun {x} {+ x {openbox a}}}}
                          {seqn {setbox a 2}
                                {f 5}}}})
      (app (fun 'a (app (fun 'f (seqn (setbox (id 'a) (num 2))
                                      (seqn (app (id 'f) (num 5)) (mtSeqn))))
                        (fun 'x (add (id 'x) (openbox (id 'a))))))
           (newbox (num 1))))
(test (parse '{with {b {newbox 0}}
                    {seqn {setbox b {+ {openbox b} 1}}
                          {setbox b {+ {openbox b} 1}}
                          {setbox b {+ {openbox b} 1}}
                          {openbox b}}})
      (app (fun 'b
                (seqn (setbox (id 'b) (add (openbox (id 'b)) (num 1)))
                      (seqn (setbox (id 'b) (add (openbox (id 'b)) (num 1)))
                            (seqn (setbox (id 'b) (add (openbox (id 'b)) (num 1)))
                                  (seqn (openbox (id 'b)) (mtSeqn))))))
           (newbox (num 0))))

;; env-lookup : symbol Env -> location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier")]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? name bound-name)
              bound-location
              (env-lookup name rest-env))]))

;; store-lookup : location Store -> BCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error 'store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= loc-index location)
              value
              (store-lookup loc-index rest-store))]))

;; interp : BCFAE Env Store -> Value*Store
(define (interp expr env store)
  (type-case BCFAE expr
    [num (n) (v*s (numV n) store)]
    [add (l r)
         (type-case Value*Store (interp l env store)
           [v*s (l-value l-store)
                (type-case Value*Store (interp r env l-store)
                  [v*s (r-value r-store)
                       (v*s (num+ l-value r-value) r-store)])])]
    [id (v) (v*s (store-lookup (env-lookup v env) store) store)]
    [fun (bound-id bound-body)
         (v*s (closureV bound-id bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case Value*Store (interp fun-expr env store)
           [v*s (fun-value fun-store)
                (type-case Value*Store (interp arg-expr env store)
                  [v*s (arg-value arg-store)
                       (local [(define new-loc (next-location arg-store))]
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
    [newbox (val-expr)
            (type-case Value*Store (interp val-expr env store)
              [v*s (val-value val-store)
                   (local [(define new-loc (next-location val-store))]
                     (v*s (boxV new-loc)
                          (aSto new-loc val-value val-store)))])]
    [setbox (box-expr val-expr)
            (type-case Value*Store (interp box-expr env store)
              [v*s (box-value box-store)
                   (type-case Value*Store (interp val-expr env box-store)
                     [v*s (val-value val-store)
                          (v*s val-value
                               (update-store (boxV-loc box-value)
                                             val-value
                                             val-store))])])]
    [openbox (box-expr)
             (type-case Value*Store (interp box-expr env store)
               [v*s (box-value box-store)
                    (v*s (store-lookup (boxV-loc box-value) box-store)
                         box-store)])]
    [seqn (fst rst)
          (if (mtSeqn? rst)
              (interp fst env store)
              (type-case Value*Store (interp fst env store)
                [v*s (fst-value fst-store)
                     (interp rst env fst-store)]))]
    [mtSeqn () (error 'interp "empty seqn")]))

;; num+ : BCFAE-Value BCFAE-Value -> BCFAE-Value
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

(test (num+ (numV 1) (numV 2)) (numV 3))

;; num-zero? : BCFAE-Value -> boolean
(define (num-zero? n)
  (zero? (numV-n n)))

(test (num-zero? (numV 0)) #t)
(test (num-zero? (numV 1)) #f)

;; next-location : Store -> number
(define (next-location store)
  (type-case Store store
    [mtSto () 0]
    [aSto (location value rest-store)
          (+ location 1)]))

;; update-store : number BCFAE-Value Store -> Store
(define (update-store loc val store)
  (type-case Store store
    [mtSto () (mtSto)]
    [aSto (location value rest-store)
          (if (= loc location)
              (aSto loc val rest-store)
              (aSto location
                    value
                    (update-store loc val rest-store)))]))

(test (update-store 100 (numV 233) (mtSto)) (mtSto))
(test (update-store 100 (numV 233) (aSto 101 (numV 0) (aSto 102 (numV 1) (mtSto))))
      (aSto 101 (numV 0) (aSto 102 (numV 1) (mtSto))))
(test (update-store 102 (numV 233) (aSto 101 (numV 0) (aSto 102 (numV 233) (mtSto))))
      (aSto 101 (numV 0) (aSto 102 (numV 233) (mtSto))))
(test (update-store 101 (numV 233) (aSto 100 (numV 0) (aSto 101 (numV 1) (aSto 102 (numV 2) (mtSto)))))
      (aSto 100 (numV 0) (aSto 101 (numV 233) (aSto 102 (numV 2) (mtSto)))))

(test (v*s-value (interp (parse '{with {d 1}
                                       {with {add-d {fun {x} {+ x d}}}
                                             {add-d 4}}})
                         (mtSub)
                         (mtSto)))
      (numV 5))
(test (v*s-value (interp (parse '{with {b {newbox 4}}
                                       {+ {openbox b}
                                          {seqn {setbox b 5}
                                                {openbox b}}}})
                         (mtSub)
                         (mtSto)))
      (numV 9))
(test (v*s-value (interp (parse '{with {a {newbox 1}}
                                       {if0 {seqn {setbox a 0}
                                                  {openbox a}}
                                            233
                                            {openbox a}}})
                         (mtSub)
                         (mtSto)))
      (numV 233))
(test (v*s-value (interp (parse '{with {b {newbox 0}}
                                       {seqn {setbox b {+ {openbox b} 1}}
                                             {setbox b {+ {openbox b} 1}}
                                             {setbox b {+ {openbox b} 1}}
                                             {openbox b}}})
                         (mtSub)
                         (mtSto)))
      (numV 3))
