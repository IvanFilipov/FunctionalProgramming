

;list remove first min -> map ( . ) -> accum find first min ->filter remove min -> map result


;task 1 : 157 -> (1 5 7)


(define (to-list n)
  (define (accum n list)
    ( if (< n 1)
         list
         (accum (quotient n 10) (cons (remainder n 10) list))
         )
    )
 (accum n '())
)


;task 2 '(1 2 1 5 2 3 1 6 4 2 5) -> (1 2 5 3 6 4)

(define (filter list pr?)
  (if (null? list)
      list
      (if (pr? (car list))
          (cons (car list) (filter (cdr list) pr?))
               (filter (cdr list) pr?)
      )
  )
 )
              
          


(define (distinct list)
  (if (null? list)
          list
       (cons (car list) (distinct (filter list (lambda (x) (not (= x (car list)))))))
   )
)


;insertion sort

(define (insert l x)
  (if (null? l)
      (cons x l)
      (if (<= (car l) x)
          (cons (car l) (insert (cdr l) x))
          (cons x l)
      )
   )
)

(define (accumulate fn res list)
  (if (null? list)
      res
      (accumulate fn (fn res (car list)) (cdr list))
      )
  )

(define (insertion-sort l)
  (accumulate insert '() l)
 )

(define (only-odd list)
  (filter list odd?)
  )


;flatten

(define (flatten l)
  (if (null? l)
         l
         (if(list? (car l))
                   (append (flatten (car l)) (flatten (cdr l)))
                   (cons (car l) (flatten (cdr l)))
          )
   )
)


; position -> (1 2 1 3 1 4 1 5 1 7 1 2) 1 -> ( 0 2 4 6 8 10)

; solution -> map ( 0 1) ( 1 2) ...
           ; -> filter lambda = x cdr p
           ; -> map lambda x car x

(define (accum list func start)
  (if (null? list)
          start
          (accum (cdr list) func (func start list))
   )
)

(define (position l n)
  (map (lambda (x) (- (car l) (length l)))
        (filter (accum l (lambda (v x) (cons (cons (length x) (car x)) v)) '())
                (lambda (x) (= (cdr x) n))
         )
   )   
)


    