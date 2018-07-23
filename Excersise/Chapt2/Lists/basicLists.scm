#lang sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;;2.17 last-pair : return the list that contains last element of a list
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

(define (reverse list)
  (if (null? (cdr list))
      (car list)
      (reverse (cdr list)) (car list)))