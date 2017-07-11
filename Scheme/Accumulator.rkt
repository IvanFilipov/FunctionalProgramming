; accumulator's body

(define (accum first-elem next-fn start-val acc-fn cond)
  (define (acc n-step cur-elem cur-val)
    (if (cond cur-elem n-step)
              cur-val
              (acc (+ n-step 1) (next-fn cur-elem n-step) (acc-fn cur-elem cur-val)) ;make the calculation
                                                                                     ;and procced to the next
    )

  )
    (acc 1 first-elem start-val) ; actual call
)


;help func
(define (next-fact cur n) (/ cur n))
(define (inc x n) (+ x 1))
(define (dec x n) (- x 1))
(define (cond-100 cur n) (> n 100))


;sum of first n numbers
(define (sum-n n)
  (define (cond-less c x) (> x n)) 
  (accum 1 inc 0 + cond-less)
)




;prime?

(define (prime x n)
  (if(>= n x )
        #t
        (if(= (remainder x n) 0)
               #f
               (prime x (+ n 1))
         )    
     
   )     
)   


(define (prime? x)

 ( if ( < x 2)
        #t
    (prime x 2)
  )
)


;count of prime numbers in [a;b]

(define (prim-cnt a b)
  (define (cond-less c x) (> x  (- b a)))
  (define (acc-fn x sum) (if(prime? x)
                             (+ sum 1)
                                sum
                          )
    )
    (accum a inc 0 acc-fn cond-less)
 )



