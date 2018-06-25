#lang racket
(define (pascals index)
  (define (get-row index)
    (define (find-row index cur-index row)
      (define (search-row count)
        (cond ((= count row) -1)
              ((= (+ cur-index (+ count 1)) index)
               (if (or (= count 0)
                       (= (+ count 1) row))
                   -2
                   row))
              (else (search-row (+ count 1))))
        )
      
      (define result (search-row 0))
      (cond ((= result -1)
             (find-row index
                       (+ cur-index row)
                       (+ row 1)))
            ((= result -2) -1)   
            (else row))
      )
    
    (if (= index 1) -1
        (find-row index 1 1))
    )

  )
  
