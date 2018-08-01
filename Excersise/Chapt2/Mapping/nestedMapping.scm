#lang sicp
; Nested Mapping

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                           (lambda (i)
                             (map (lambda (j) (list i j))
                                  (enumerate-interval 1 (- i 1))))
                           (enumerate-interval 1 n)))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

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

;;given are above
(define (prime? n)
  (define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
           (remainder
            (square (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base (expmod base (- exp 1) m))
            m))))
  (define (fast-prime-smart? times)
    (define (miller-rabin n)
      (define (new-square a)
        (define result (* a a))
        (cond ((= a 1) 1)
              ((= a (- n 1)) result)
              ((= result 1) 0)
              (else result))
        )
      (define (expmodded base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
               (remainder
                (new-square (expmodded base (/ exp 2) m))
                m))
              (else
               (remainder
                (* base (expmodded base (- exp 1) m))
                m))))
      (define (try-it a)
        (not (= (expmod a n n) 0))
        )
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0) true)
          ((miller-rabin n) (fast-prime-smart? n (- times 1)))
          (else false)))
  (fast-prime-smart? 5))
;;2.40 define unique-pairs to simplify prime-sum-pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1)))
     (enumerate-interval 1 n))))


