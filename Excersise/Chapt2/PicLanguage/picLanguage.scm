#lang sicp
; Picture Language

;excersise 2.46 working with vectors
(define (make-vect xcor ycor)
  (cons xcor ycor))
(define (xcor vect)
  (car vect))
(define (ycor vect)
  (cdr vect))
(define (add-vect v1 v2)
  (make-vect (+ (xcor v1) (xcor v2)) (+ (ycor v1) (ycor v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor v1) (xcor v2)) (- (ycor v1) (ycor v2))))

