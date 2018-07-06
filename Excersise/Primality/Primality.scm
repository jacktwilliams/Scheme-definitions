#lang sicp
;;below: given in book
(define (square n)
  (* n n))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
(= n (smallest-divisor n)))


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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;given for excersise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;;my procedure for excersise 1.22
(define (search-for-primes floor ceiling)
  (define (search n)
    (timed-prime-test-beauty n)
    (if (> n ceiling) (display "XXX Ceiling XXX")
        (search (+ n 2)))
    )
  (if (even? floor) (search (+ floor 1))
      (search floor)))

;;also gonna refactor timed-prime-test for less printing..
(define (timed-prime-test-beauty n)
  (define (start-prime-test n start-time)
  (if (fast-prime? n 4)
      (report-prime (- (runtime) start-time)))
    )
  (define (report-prime elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time)
    )
  (start-prime-test n (runtime)))

;;excersise 1.23 eliminate needless testing
(define (next test-divisor)
  (if (= test-divisor 2) 3
      (+ test-divisor 2)))

;;excersise 1.27
(define (inef-prime? n)
  (define (itr current)
    (if (= current 1) true
        (if (= current (expmod current n n))
            (itr (- current 1)) ;;should test all odds and then 2.
            false)))
  (itr (- n 1)))

;;excersise 1.28
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

(define (fast-prime-smart? n times)
  (cond ((= times 0) true)
        ((miller-rabin n) (fast-prime-smart? n (- times 1)))
        (else false)))



