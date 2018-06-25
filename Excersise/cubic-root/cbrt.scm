#lang sicp
(define (square x)
  (* x x))

(define (avg x y)
 (/ (+ x y) 2))

(define (abs x)
 (if (< x 0) (- x)
     x))

(define (improve x guess)
 (avg guess (/ x guess)))

(define (good-enough? guess prev-guess)
 (< (abs (- guess prev-guess)) .0001))

(define (sqrt-iter x guess prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (sqrt-iter x (improve x guess) guess)))

(define (sqrt x)
  (sqrt-iter x 1.0 0))

(define (improve-cb x guess)
  (/ (+ (* guess 2)
        (/ x (square guess)))
     3))

(define (cbrt-iter x guess prev-guess)
  (if (good-enough? guess prev-guess)
      guess
      (cbrt-iter x (improve-cb x guess) guess)))

(define (cbrt x)
  (cbrt-iter x 1.0 0))

