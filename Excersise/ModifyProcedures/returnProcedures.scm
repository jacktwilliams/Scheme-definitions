#lang sicp
;;1.3.4
(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define tolerance .0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x)) newton-transform 1.0))

(define (square x)
  (* x x))
(define (cube x)
  (* x x x))

;;1.40 exc. Define cubic
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;excersise 1.41 define double which takes a procedure of one agrument as argument and applies it twice
(define (double prod)
  (lambda (x) (prod (prod x))))

;;excersise 1.42 define comp (composition f(g(x)))
(define (compose f g)
  (lambda (x) (f(g x))))

;;excersise 1.43 repeated application
(define (repeated func n)
  (cond
    ((= n 1) func)
    ((= n 2) (compose func func))
    (else (compose func (repeated func (- n 1))))))

;;excersise 1.44 smoothing functions
;;need a three-way average
(define (three-avg x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (three-avg (f (- x dx))
                         (f x)
                         (f (+ x dx)))))
;;n-fold smoothing
(define (nsmooth f n)
  (repeated smooth n))