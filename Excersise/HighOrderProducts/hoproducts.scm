#lang racket
;;excersise 1.31 create product higher order procedure
;;which finds the product of values of a function at points over given range
(define (prod f ceiling interv current)
  (if (> current ceiling)
      1
      (* (f current)
         (prod f ceiling interv (+ current interv)))))

;;test by showing how it works with factorial
(define (fact n)
  (define (identity x) x)
  (new-prod identity n 1 1))

;;lets approximate pi
;;works. I just need to know how to convert fraction to decimal.
(define (get-pi)
  (define (itr index value second? n)
      (if (= index n)
          value
          (if second?
              (itr (+ index 1) (+ value 2) (not second?) n)
              (itr (+ index 1) value (not second?) n)))
      )
  (define (get-numer n)
    (if (= n 1)
        2
        (itr 2 4 #f n))
    )
  (define (get-denom n)
    (if (= n 1)
        3
        (itr 2 3 #t n))
    )
  ;;dividing numerator sequence by denom sequence approximates pi/4
  (* 4
     (/ (prod get-numer 100 1 1)
        (prod get-denom 100 1 1))))

;;my prod function uses a linear recursive definition. Let's try an iterative
(define (new-prod f ceiling interv current)
  (define (p-itr current result)
    (if (= current ceiling)
        (* result (f current))
        (p-itr (+ current 1) (* result (f current))))
    )
  (p-itr 1 1))
            