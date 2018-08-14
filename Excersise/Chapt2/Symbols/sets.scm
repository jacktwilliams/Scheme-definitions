#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;excersise 2.59 implement union-set
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (if (element-of-set? (car set1) set2)
          (union-set (cdr set1) set2)
          (cons (car set1) (union-set (cdr set1) set2)))))

;excersise 2.60 implement procedures for a representation that allows duplicates
(define (element-of-set1? x set)
  (element-of-set x set))
(define (adjoin-set1 x set)
  (cons x set))
(define (union-set1 set1 set2)
  (if (null? set1) set2
      (cons (car set1) (union-set (cdr set1) set2))))
(define (intersection-set1 set1 set2)
  (intersection-set set1 set2))
;Performance increases for adjoin-set and union-set. Memory use is increased.



        