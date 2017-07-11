(define (accum list func start)
  (if (null? list)
          start
          (accum (cdr list) func (func start (car list)))
   )
)


(define (minimal list)
  (accum list (lambda (x y)
                (if(< x y)
                    x
                    y
                 )
                )
         (car list)
         )
  )


(define (id x) x)
(define (sq x) (* x x))

(define (compose f g) (lambda (x) (f (g x))))


(define (select-fn x)
  (if (> x 0)
    (lambda (y) (+ 1 y))
    (lambda (y) (- y x))
   )
)


