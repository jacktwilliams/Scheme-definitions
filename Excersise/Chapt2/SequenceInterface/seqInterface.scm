#lang sicp
;; Sequences as Conventional Interfaces

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

;;procedures above are given

;;2.33. Procedure are given with instruction to fill in blanks to complete basic list
;;manipulation operations as accumulations

;;(define (map p sequence)
  ;;(accumulate (lambda (x y)
               ;; (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate
   (lambda (x y) (+ 1 y)) 0 sequence))

(define test-seq (list 1 2 3 4 5 (list 1 2)))
(define (square x)
  (* x x))
(define test-seq1 (list 1 2 3 4 5 (list 1 2 3)))

;;2.34 evaluating polynomials using horner's rule
;; not working correctly..
(define (horner-eval x coefficient-sequence)
  (let ((initial (car coefficient-sequence))
        (seq (cdr coefficient-sequence)))
    (accumulate (lambda (this-coeff higher-terms)
                  (if (null? higher-terms)
                      (+ (* this-coeff x) higher-terms)
                      (* x (+ this-coeff higher-terms))))
                initial
                seq)))

;;2.35 redefining count leaves in terms of accumulate
(define (count-leaves t)
  (accumulate
   (lambda (current rest)
     (if (pair? current)
         (+ (count-leaves current) rest)
         (+ 1 rest)))
   0
   t))

;;finishing given definition for accumulate-n. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (get-firsts seqs))
            (accumulate-n op init (get-not-firsts seqs)))))

(define (get-firsts seqs)
  (define (get-first seq)
    (car seq))
  (if (null? seqs)
      nil
      (cons (get-first (car seqs)) (get-firsts (cdr seqs)))))

(define (get-not-firsts seqs)
  (define (get-not seq)
    (cdr seq))
  (if (null? seqs)
      nil
      (cons (get-not (car seqs))
            (get-not-firsts (cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define test-seqs (list (list 1 2 3) (list 1 2 3)))
