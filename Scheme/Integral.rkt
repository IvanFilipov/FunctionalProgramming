; Integral calc using Riman's sums

(define (integral f a b)
       (let ((h 0.0001))
             ( if(>= a b)
                    0
                   (+ (* (f (+ a (/ h 2))) h ) (integral f (+ a h) b)                             )
              )
       )                      
)

(define (const x) 2)
(define (id x) x)
(define (sq x) (* x x))
(define (pol x) (+ (* x 5) 4))

