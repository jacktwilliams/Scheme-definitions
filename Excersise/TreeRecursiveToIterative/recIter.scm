#lang racket
(define (f-recursive n)
  (cond ((< n 3) n)
        (else (+ (f-recursive(- n 1))
                 (* (f-recursive(- n 2)) 2)
                 (* (f-recursive(- n 3)) 3)))))

(define (f-iter n)
  (define (itr c b a count)
    (cond ((= count n) a)
          (else (itr b a (+ a (* 2 b) (* 3 c)) (+ count 1)))))
  (itr 0 1 2 2))
                 
