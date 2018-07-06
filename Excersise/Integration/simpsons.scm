#lang sicp
;;excersise 1.29 Simpson's rule for integrating function f between a and b.
(define (even? x)
  (= (remainder x 2) 0))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f floor ceiling n)
  (define (term c)
    (define y (/ (- ceiling floor) n))
    (define base (f (+ floor (* c
                                (/ (- ceiling floor) n))))
      )
    (cond ((or (= c 0) (= c n)) base)
          ((even? c) (* 2 base))
          (else (* 4 base))))
  (define h (/ (- ceiling floor) n))
  (* (/ h 3.0)
     (sum term 0 inc n)))
  
(define (cube a)
  (* a a a))
    
  