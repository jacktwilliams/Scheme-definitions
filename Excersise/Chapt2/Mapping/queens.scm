#lang sicp
; eight-queens puzzle
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        empty-board
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

; excersise 2.42
(define (make-position row col)
  (cons row col))
(define (get-row place)
  (car place))
(define (get-col place)
  (cdr place))

(define empty-board nil)

(define (adjoin-position row col positions)
  (if (null? positions)
      (list (make-position row col))
      (append positions (list (make-position row col)))))

(define (safe? k established)
  (define (get-row-colk positions)
    (if (null? positions)
        (error "No queen in column" k)
        (if (= k (get-col (car positions)))
            (get-row (car positions))
            (get-row-colk (cdr positions)))))
  (define (occupied? new-row positions)
    ;(let ((current-position-row (get-row (car positions))))
    (if (null? positions)
        #t
        (if (and (or (= new-row (get-row (car positions)))    ;check diagnols
                     (= (+ new-row 1) (get-row (car positions)))
                     (= (- new-row 1) (get-row (car positions))))
                 (not (= k (get-col (car positions)))))
            #f
            (occupied? new-row (cdr positions)))))
  (occupied? (get-row-colk established) established))

(define test-safe (safe? 1 (adjoin-position 1 1 empty-board)))
(define test2-safe (safe? 2 (adjoin-position 1 2 (adjoin-position 1 1 empty-board))))
(define test3-safe (safe? 2 (adjoin-position 2 2 (adjoin-position 1 1 empty-board)))) ; should fail to diagnol case
(define test4-safe (safe? 2 (adjoin-position 3 2 (adjoin-position 1 1 empty-board)))) ; should pass
(define sample (adjoin-position 1 3 empty-board))
(define adv-sample (adjoin-position 2 2 (adjoin-position 1 1 empty-board)))