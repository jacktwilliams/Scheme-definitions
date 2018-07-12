#lang sicp
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

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;;excersise 2.1. make-rat should handle negatives
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((and (negative? n) (negative? d)) (cons (/ (- n) g) (/ (- d) g)))
          ((or (negative? n) (negative? d)) (cons (-(/ n g)) (/ d g)))
          (else (cons (/ n g) (/ d g))))))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

;;excersise 2.2 representing lines
(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))

(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint segment)
  (make-point (div-rat (add-rat (x-point (start-segment segment))
                          (x-point (end-segment segment)))
                       (make-rat 2 1))
              (div-rat (add-rat (y-point (start-segment segment))
                          (y-point (end-segment segment)))
                       (make-rat 2 1))))
(define testSeg (make-segment (make-point (make-rat 2 1) (make-rat 2 1)) (make-point (make-rat 4 1) (make-rat 4 1))))

;;2.3 representing rectangles in a plane
;;rep 1. Two lines
(define (make-rect vert horiz)
  (cons vert horiz))

(define (horiz rect)
  (car rect))
(define (vert rect)
  (cdr rect))

(define (perim rect)
  ((let ((vert-distance (sqrt (/ (numer