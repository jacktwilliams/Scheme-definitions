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

;;excersise 2.20. Given arguments, return list of all arguments with same even-odd parity of first argument.
;;helper func test parity of two ints
(define (same-par x y)
  (if (or (and (even? x) (even? y))
          (and (not (even? x)) (not (even? y))))
      #t
      #f))

(define (same-parity1 first . list)
  (define (par-itr l n)
    (if (= n 0)
        (if (same-par first (car l))
            (append (cdr l) (cons (car l) nil))
            (cdr l))
        (if (same-par first (car l))
            (par-itr (append (cdr l) (cons (car l) nil)) (- n 1))
            (par-itr (cdr l) (- n 1)))))
  (par-itr (append (cons first nil) list) (length list)))
  
    