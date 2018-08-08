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
(define (scale-vect x vect)
  (make-vect (* x (xcor vect)) (* x (ycor vect))))

;excersise 2.47 selectors for two different frame constructors
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin frame)
  (car frame))
(define (edge1 frame)
  (cadr frame))
(define (edge2 frame)
  (caddr frame))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;only needs a new edge 2 selector
;(define (edge2 frame)
;  (cddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))



