#lang sicp
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
  (acc-prod identity n 1))

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

;;more general notion of accumulate. Excersise 1.33
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;;summation in terms of accumulate
(define (summation a b term next)
  (define (plus a b)
    (+ a b)
    )
  (accumulate plus 0 term a next b))

;;testing
(define (intSum n)
  (define (identity x)
    x
    )
  (summation 1 n identity inc))

;;products in terms of accumulate
(define (acc-prod f ceiling interv)
  (define (mult x y)
    (* x y)
    )
  (define (next x)
    (+ x interv))
  (accumulate-it mult 1 f 1 next ceiling))

;;iterative accumulate
(define (accumulate-it combiner null-value term a next b)
  (define (acc-iter x result)
    (if (> x b)
        (combiner result null-value)
        (acc-iter (next x) (combiner result (term x))))
    )
  (acc-iter a null-value))
            