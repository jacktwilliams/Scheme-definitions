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


  