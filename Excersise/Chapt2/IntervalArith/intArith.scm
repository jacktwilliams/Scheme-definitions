#lang sicp
(define (make-interval a b) (cons a b))

(define (lower-bound int) (car int))

(define (upper-bound int) (cdr int))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;;2.10 check for intervals that span 0 (div modification)
(define (div-interval x y)
  (let ((low-err (and (negative? (lower-bound x)) (not (negative? (upper-bound x)))))
        (up-err (and (negative? (lower-bound y)) (not (negative? (upper-bound y))))))
    (if (or low-err up-err)
        (error "Cannot divide when intervals span 0.")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y)))))))

;;excersise 2.8 sub-interval
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;;given
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;;excersise 2.12 constructor that makes interval with tolerance percentage
(define (make-center-percent c tol)
  (make-interval (- c (* c (/ tol 100))) (+ c (* c (/ tol 100)))))

;;2.12 selector 'percent' which finds the tolerance percentage
(define (percent int)
  (* 100 (/ (width int) (center int))))

(define A (make-center-width 5 .1))
(define B (make-center-width 10 .1))
(define aa (div-interval A A))
(define ab (div-interval A B))