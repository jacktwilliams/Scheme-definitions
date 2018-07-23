#lang sicp
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;;2.17 last-pair : return the list that contains last element of a list
(define (last-pair list)
  (if (null? (cdr list))
      (car list)
      (last-pair (cdr list))))

;;2.18 reverse the list
(define (reverse list)
   (define (reverse-proc l orig-first)
     (if (null? (cdr l))
         (car l)
         (if orig-first
             (cons (reverse-proc (cdr l) #f) (cons (car l) nil))
             (cons (reverse-proc (cdr l) #f) (car l)))))
  (reverse-proc list #t))

(define back-list (reverse (list 1 2 3 4 5)))