#lang sicp

;;excersise 2.28. Print all leaves of a tree in left to right order
(define (fringe l)
  (if (null? (cdr l))
      (if (pair? (car l))
          (fringe (car l))
          (list (car l)))
      (if (pair? (car l))
          (append (fringe (car l)) (fringe (cdr l)))
          (append (list (car l)) (fringe (cdr l))))))

(define x (list (list 1 2) (list 3 4)))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;2.29 binary mobiles
(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mob)
  (car mob))

(define (right-branch mob)
  (cdr mob))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cdr b))

(define testb (make-branch 0 1))
(define testBR (make-branch 1 testb))
(define testMob (make-mobile testb testBR))

(define (total-weight mob)
  (define (total branch)
    (if (= (branch-length branch) 0)
        (car (branch-structure branch))
        (total-weight (branch-structure branch))))
  (+ (total (left-branch mob)) (total (right-branch mob))))
  