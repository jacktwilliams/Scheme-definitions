#lang sicp
; Nested Mapping
; Import prime?
;;given are above
(define (prime? n)
  (define (square n)
    (* n n))
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
          ((miller-rabin n) (fast-prime-smart? (- times 1)))
          (else false)))
  (fast-prime-smart? 5))

; Given

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

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
;;2.40 define unique-pairs to simplify prime-sum-pairs
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
     (enumerate-interval 1 n)))

;; 2.41 Write a procedure to find all ordered triples
;of distinct positive integers i, j, and k less than or equal to
;a given integer n that sum to a given integer s

(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
            (map (lambda (k)
                   (list i j k))
                 (enumerate-interval 1 (- j 1))))
            (enumerate-interval 1 (- i 1))))
   (enumerate-interval 3 n)))

(define (make-triple-sum triple)
  (list (car triple) (cadr triple) (caddr triple)
        (+ (car triple) (cadr triple) (caddr triple))))

(define (triples-with-sum max-int sum)
  (map make-triple-sum
       (filter (lambda (trip)
                 (= sum (+ (car trip) (cadr trip) (caddr trip))))
               (unique-triples max-int))))
       
          


