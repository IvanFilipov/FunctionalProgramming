(define (fact x) ( if(< x 2)
                     1
                     (* x (fact(- x 1)))
                 )
)