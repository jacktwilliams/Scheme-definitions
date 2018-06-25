#lang racket
(define (pascals index)
  (define (get-row)
    (define (find-row cur-index row)
      (define (search-row count)
        (cond ((= count row) -1)
              ((= (+ cur-index count) index)
               (if (or (= count 0)
                       (= count (- row 1)))
                   -2
                   row))
              (else (search-row (+ count 1))))
        )
      
      (define result (search-row 0))
      (cond ((= result -1)
             (find-row (+ cur-index row)
                       (+ row 1)))
            ((= result -2) -1)   
            (else row))
      )
    
    (if (= index 1) -1
        (find-row 2 2))
    )

  (define row (get-row))
  (cond ((= row -1) 1)
        (else (+ (pascals (- index row))
                 (pascals (- index (- row 1))))))
  )



  
