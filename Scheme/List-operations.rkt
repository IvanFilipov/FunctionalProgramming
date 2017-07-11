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

(define (inc x unused) (+ 1 x))


;creates a list with elems form a to b
(define (make-list a b)
  (define (less a n) (> a b))
  (reverse (accum a inc '() cons less))

 )


(define (len l)
     (if (null? l)
         0
        (+ 1 (len (cdr l)))
     )
)

(define (search l e)
  (if (null? l)
         #f
        (if (= e (car l))
               #t
              (search (cdr l) e)
        )
   )
 )

(define (first-n-of-list res n l)
  (if (or (= n 0) (null? l))
        (reverse res)
        (first-n-of-list (cons (car l) res) (- n 1) (cdr l))
  )
)


(define (push-back x l)
  (if (null? l)
      (cons x l)
      (reverse(cons x (reverse l)))
      )
 )
