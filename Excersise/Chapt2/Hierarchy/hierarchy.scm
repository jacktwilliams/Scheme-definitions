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
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mob)
  (car mob))

(define (right-branch mob)
  (cdr mob))

(define (branch-length b)
  (car b))
(define (branch-structure b)
  (cdr b))

(define testb (make-branch 1 1))
(define testBR (make-branch 1 (make-mobile testb testb)))
(define testMob (make-mobile testBR testBR))

(define (total-weight mob)
  (define (total branch)
    (if (= (branch-length branch) 1)
        (branch-structure branch)
        (total-weight (branch-structure branch))))
  (+ (total (left-branch mob)) (total (right-branch mob))))

(define (holds-mob? branch)
  (pair? (branch-structure branch)))

(define (balanced? mob)
  (let ((left-has (holds-mob? (left-branch mob)))
        (right-has (holds-mob? (right-branch mob))))
    (if (and left-has right-has)
        (and (= (torque (left-branch mob)) (torque (right-branch mob)))
             (and (balanced? (branch-structure (left-branch mob)))
                  (balanced? (branch-structure (right-branch mob)))))
        (if left-has
            (and (= (torque (left-branch mob)) (torque (right-branch mob)))
                 (balanced? (branch-structure (left-branch mob))))
            (if right-has
                (and (= (torque (left-branch mob)) (torque (right-branch mob)))
                     (balanced? (branch-structure (right-branch mob))))
                ;;neither hold mobiles
                (= (torque (left-branch mob)) (torque (right-branch mob))))))))

(define (torque branch)
  (if (holds-mob? branch)
      (* (branch-length branch) (total-weight (branch-structure branch)))
      (* (branch-structure branch) (branch-length branch))))
