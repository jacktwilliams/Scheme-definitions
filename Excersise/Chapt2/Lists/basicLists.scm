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
  (if (null? (cdr list))
      (cons (car list) nil)
      (append (reverse (cdr list)) (cons (car list) nil))))

(define back-list (reverse (list 1 2 3 4 5)))

;;excersise 2.19. Modifying count change to use lists
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define (except-first-denomination list)
  (cdr list))
(define (first-denomination list)
  (car list))
(define (no-more? list)
  (null? list))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (even? n)
  (= 0 (remainder n 2)))

(define (same-parity first . list)
  (define (construct l)
    (if (null? l)
        nil
        (if (or (and (even? first) (even? (car l)))
                (and (not (even? first)) (not (even? (car l)))))
            (append (cons (car l) nil) (same-parity (cdr l)))
            (same-parity (cdr l)))))
  (construct list))