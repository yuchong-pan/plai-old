#lang plai

(define (route-producer-body send)
  (begin (send 'providence)
         (send 'houston)
         (send 'bangalore)))

(define (odds-producer-body send)
  (local [(define (loop n)
            (begin (send n)
                   (loop (+ n 2))))]
    (loop 1)))

(define (general-producer body)
  (local [(define resume (box false))]
    (lambda (real-send)
      (local [(define send-to (box real-send))
              (define send (lambda (value-to-send)
                             (set-box! send-to
                                       (let/cc k
                                         (begin (set-box! resume k)
                                                ((unbox send-to) value-to-send))))))]
        (if (unbox resume)
            ((unbox resume) real-send)
            (body send))))))

(define get call/cc)
(define route-producer (general-producer route-producer-body))
(define odds-producer (general-producer odds-producer-body))
