#lang sicp
;; Sequences as Conventional Interfaces

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;;procedures above are given

;;2.33. Procedure are given with instruction to fill in blanks to complete basic list
;;manipulation operations as accumulations

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate
   (lambda (x y) (+ 1 y)) 0 sequence))

(define test-seq (list 1 2 3 4 5 (list 1 2)))
(define (square x)
  (* x x))
(define test-seq1 (list 1 2 3 4 5))