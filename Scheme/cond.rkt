(define (cond x y) (if ( > (* x y) 0 )
                       (if ( > x 0)
                              1
                              3
                       )
                       (if ( > x 0)
                              4
                              2
                       )
                   )
  )