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
  (element-of-set? x set))
(define (adjoin-set1 x set)
  (cons x set))
(define (union-set1 set1 set2)
  (if (null? set1) set2
      (cons (car set1) (union-set (cdr set1) set2))))
(define (intersection-set1 set1 set2)
  (intersection-set set1 set2))
;Performance increases for adjoin-set and union-set. Memory use is increased.
; Ordered sets

(define (element-of-set-or? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set-or? x (cdr set)))))

(define (intersection-set-or set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-set-or (cdr set1)
                                             (cdr set2))))
              ((< x1 x2)
               (intersection-set-or (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-or set1 (cdr set2)))))))
;excersise 2.61
(define (adjoin-set-or x set)
  (cond ((null? set) nil)
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        ((> x (car set)) (cons (car set) (adjoin-set-or x (cdr set))))))

;excersie 2.62 ordered sets O(n) union
(define (union-set-or set1 set2)
  (cond ((null? set2) set1)
        ((null? set1) set2)
        (else (union-set-or (cdr set1) (adjoin-set-or (car set1) set2)))))

; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))





