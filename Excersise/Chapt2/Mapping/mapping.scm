#lang sicp

(define (map proc items)
  (if (null? items)nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

;;excersise 2.21. Finish two implementations of square-list
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list1 items)
  (map (lambda (x) (* x x)) items))

(define (square x)
  (* x x))

;;excersise 2.23 implement for-each
(define (for-each proc list)
  (cond ((null? list) #t)
        (else (proc (car list))
              (for-each proc (cdr list)))))