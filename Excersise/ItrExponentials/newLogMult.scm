#lang racket
;;design a multiplication procedure (pretending no such procedure exists)
;;logarithmic time, using 'double' and 'half'
;; excersise 1.17
(define (double a)
  (+ a a)
  )
(define (halve a)
  (/ a 2)
  )
(define (even? a)
  (= (remainder a 2) 0)
  )
(define (mult a b)  
  (cond ((= b 0) 0)
        ((even? b) (mult (double a) (halve b)))
        (else (+ a (mult a (- b 1))))))

;;excersise 1.18 Designing an iterative version of the above algorithm.
(define (itr-mult b c)
  (define (itr a b c)
    ;;mult b * c
    (cond ((= c 0) a)
          ((even? c) (itr (+ a (double b))
                          b
                          (halve c)))
          (else (itr (+ a b) b (- c 1))))
    )
  (itr 0 b c))