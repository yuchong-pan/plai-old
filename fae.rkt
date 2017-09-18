#lang plai

;; pre-process : sexp -> sexp
(define (pre-process sexp)
  (cond [(number? sexp) sexp]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) '+))
         (list '+
               (pre-process (second sexp))
               (pre-process (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'with)
              (list? (second sexp))
              (= (length (second sexp)) 2)
              (symbol? (first (second sexp))))
         (list (list 'fun
                     (list (first (second sexp)))
                     (pre-process (third sexp)))
               (pre-process (second (second sexp))))]
        [(symbol? sexp) sexp]
        [(and (list? sexp)
              (= (length sexp) 3)
              (eq? (first sexp) 'fun)
              (list? (second sexp))
              (= (length (second sexp)) 1)
              (symbol? (first (second sexp))))
         (list 'fun
               (list (first (second sexp)))
               (pre-process (third sexp)))]
        [(and (list? sexp)
              (= (length sexp) 2))
         (list (pre-process (first sexp))
               (pre-process (second sexp)))]))

(test (pre-process '3) '3)
(test (pre-process '{+ 3 {+ 4 x}}) '{+ 3 {+ 4 x}})
(test (pre-process '{with {x 3} {+ x x}}) '{{fun {x} {+ x x}} 3})
(test (pre-process 'x) 'x)
(test (pre-process '{fun {x} {with {y {+ x x}} {+ y y}}})
      '{fun {x} {{fun {y} {+ y y}} {+ x x}}})
(test (pre-process '{with {x 1} {{fun {y} {+ y x}} 3}})
      '{{fun {x} {{fun {y} {+ y x}} 3}} 1})
