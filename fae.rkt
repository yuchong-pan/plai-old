#lang plai

(define-type FAE
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?) (body FAE?) (ds DefrdSub?)])

;; num-sexp? : sexp -> boolean
(define (num-sexp? sexp)
  (number? sexp))

;; add-sexp? : sexp -> boolean
(define (add-sexp? sexp)
  (and (list? sexp)
       (= (length sexp) 3)
       (eq? (first sexp) '+)))

;; with-sexp? : sexp -> boolean
(define (with-sexp? sexp)
  (and (list? sexp)
       (= (length sexp) 3)
       (eq? (first sexp) 'with)
       (list? (second sexp))
       (= (length (second sexp)) 2)
       (symbol? (first (second sexp)))))

;; id-sexp? : sexp -> boolean
(define (id-sexp? sexp)
  (symbol? sexp))

;; fun-sexp? : sexp -> boolean
(define (fun-sexp? sexp)
  (and (list? sexp)
       (= (length sexp) 3)
       (eq? (first sexp) 'fun)
       (list? (second sexp))
       (= (length (second sexp)) 1)
       (symbol? (first (second sexp)))))

;; app-sexp? : sexp -> boolean
(define (app-sexp? sexp)
  (and (list? sexp)
       (= (length sexp) 2)))

;; pre-process : sexp -> sexp
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
                                (pre-process (second sexp)))]))

(test (pre-process '3) '3)
(test (pre-process '{+ 3 {+ 4 x}}) '{+ 3 {+ 4 x}})
(test (pre-process '{with {x 3} {+ x x}}) '{{fun {x} {+ x x}} 3})
(test (pre-process 'x) 'x)
(test (pre-process '{fun {x} {with {y {+ x x}} {+ y y}}})
      '{fun {x} {{fun {y} {+ y y}} {+ x x}}})
(test (pre-process '{with {x 1} {{fun {y} {+ y x}} 3}})
      '{{fun {x} {{fun {y} {+ y x}} 3}} 1})

;; parse : sexp -> FAE
(define (parse sexp)
  (cond [(num-sexp? sexp) (num sexp)]
        [(add-sexp? sexp) (add (parse (second sexp))
                               (parse (third sexp)))]
        [(id-sexp? sexp) (id sexp)]
        [(fun-sexp? sexp) (fun (first (second sexp))
                               (parse (third sexp)))]
        [(app-sexp? sexp) (app (parse (first sexp))
                               (parse (second sexp)))]))

(test (parse '3) (num 3))
(test (parse '{+ 3 {+ 4 x}}) (add (num 3) (add (num 4) (id 'x))))
(test (parse 'x) (id 'x))
(test (parse '{fun {x} {+ x x}}) (fun 'x (add (id 'x) (id 'x))))
(test (parse '{{{fun {x} x} {fun {x} {+ x 1}}} 3}) (app (app (fun 'x (id 'x))
                                                             (fun 'x (add (id 'x) (num 1))))
                                                        (num 3)))

;; lookup : symbol DefrdSub -> FAE-Value
(define (lookup name ds)
  (cond [(mtSub? ds) (error 'lookup "no binding for identifier")]
        [(symbol=? name (aSub-name ds)) (aSub-value ds)]
        [else (lookup name (aSub-ds ds))]))

(test (lookup 'x (aSub 'y (numV 3) (aSub 'x (closureV 'x (add (id 'x) (id 'x)) (mtSub)) (mtSub))))
      (closureV 'x (add (id 'x) (id 'x)) (mtSub)))

;; add-numbers : FAE-Value FAE-Value -> FAE-Value
(define (add-numbers a b)
  (numV (+ (numV-n a) (numV-n b))))

;; interp : FAE DefrdSub -> FAE-Value
(define (interp expr ds)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (add-numbers (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body) (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local [(define fun-val (interp fun-expr ds))]
           (if (closureV? fun-val)
               (interp (closureV-body fun-val)
                       (aSub (closureV-param fun-val)
                             (interp arg-expr ds)
                             (closureV-ds fun-val)))
               (error 'interp "failed to yield a closure")))]))

(test (interp (parse (pre-process '{{with {delta 1} {fun {x} {+ x delta}}}
                                    {with {double {fun {x} {+ x x}}} {double 2}}}))
              (mtSub))
      (numV 5))
