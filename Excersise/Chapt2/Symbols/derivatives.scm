#lang sicp

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;excersise 2.54 define equal?
(define (equal? q1 q2)
  (if (not (and (pair? q1) (pair? q2)))
      (if (eq? q1 q2)
          #t
          #f)
      (if (or (pair? q1) (pair? q2))
          #f
          (and (equal? (car q1) (car q2)) (equal? (cdr q1) (cdr q2))))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-exponential (base exp) (- (exponent exp) 1))))
        (else
         (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (expt b n)
  (define (iter-expt a b n)
    (define (even? a)
      (= (remainder n 2) 0))
    (define (square a)
      (* a a))
    (cond ((= n 1) a)
          ((even? n) (iter-expt (* (square b) a)
                                b
                                (/ n 2)))
          (else (iter-expt (* b a)
                           b
                           (- n 1)))))
  (iter-expt 1 b n))

(define (=number? var num)
  (and (number? var) (= var num)))

;excersise 2.56 add exponentiation to given 'deriv' procedure
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base expon)
  (cadr expon))
(define (exponent expon)
  (caddr expon))
(define (make-exponential base exp)
  (cond ((=number? exp 0) 1)
        ((=number? base 1) 1)
        ((and (number? exp) (number? base)) (expt base exp))
        (else (list '** base exp))))

;excersise 2.57 change selectors for sum and product such that deriv will work with arbitray numbers of terms for adding or multiplying operations
(define (augend x)
  (list '+ (cddr x)))
(define (multiplicand x)
  (list '* (cddr x)))