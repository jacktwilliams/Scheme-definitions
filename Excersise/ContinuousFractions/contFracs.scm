#lang sicp
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))
(define (average x y)
  (/ (+ x y)
     2))

(define (close-enough? x y)
  (< (abs (- x y)) .002))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))

(define tolerance 0.00001)
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
;;procedures above this comment were provided

;;excersise 1.37. Evaluating k-term finite fractions
(define (cont-frac n d k)
  (define (itr count)
    (cond ((= count k) 1)
          (else (+ (d (- count 1))
                   (/ (n count)
                      (itr (+ count 1))))))
    )
  (/ (n 1)
     (itr 2)))

;;1.38 approx e with Euler's expansion
(define (e)
  (define (n i)
    1)
  (define (d i)
    (define (d-itr count b-one b-two val)
      (cond ((= count i)
             (cond ((or b-one b-two) 1)
                   (else val)))
            (else (cond (b-one (d-itr (+ count 1) #f #t val))
                        (b-two (d-itr (+ count 1) #f #f val))
                        (else (d-itr (+ count 1) #t #f (+ val 2))))))
      )
    (if (= i 1) 1
        (d-itr 2 #f #f 2))
    )
  (+ 2 (cont-frac n d 12)))

;;1.39 approximating tangent of x in radians using a continuous fraction
;;first a modified cont-frac which takes a combinator
;;not producing correct answer, pls review
(define (cont-frac-mod n d k comb)
  (define (itr count)
    (cond ((= count k) 1)
          (else (comb (d (- count 1))
                   (/ (n count)
                      (itr (+ count 1))))))
    )
  (/ (n 1)
     (itr 2)))
(define (tan-cf x k)
  (define (d i)
    (define (d-itr val count)
      (if (= count i) val
          (d-itr (+ val 2) (+ count 1)))
      )
    (if (= i 1) 1
        (d-itr 3 2))
    )
  (define (cont-frac-tan d k comb)
    (define (itr count)
      (cond ((= count k) 1)
          (else (comb (d (- count 1))
                   (/ (* x x)
                      (itr (+ count 1))))))
      )
    (/ x (itr 2))
    )
  (cont-frac-tan d k (lambda (x y) (- x y))))