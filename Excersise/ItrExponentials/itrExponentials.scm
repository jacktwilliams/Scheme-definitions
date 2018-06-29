#lang racket
;; excersise 1.16 1.2.4. Develop an iterative process for computing exponentials in O(logn).
(define (expt b n)
  (define (iter-expt a b n)
    (define (even? a)
      (= (remainder n 2) 0)
    )
    (define (square a)
      (* a a)
    )
    
    (cond ((= n 1) a)
          ((even? n) (iter-expt (* (square b) a)
                                b
                                (/ n 2)))
          (else (iter-expt (* b a)
                           b
                           (- n 1))))
    )
  
  (iter-expt 1 b n))
