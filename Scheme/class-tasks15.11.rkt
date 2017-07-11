#lang racket

(define (GetAtMatrix list i)
  (if (null? list)
       #f
       (if (= i 0)
           (car list)
           (GetAtMatrix (cdr list) (- i 1))
        )
   )
)


(define (select_nn cur max matrix)
  (if (= cur max)
       '()
       (cons (GetAtMatrix (car matrix) cur)
             (select_nn (+ 1 cur) max (cdr matrix))
       )
   )
)



(define (MatrixDiag list)
  (if (null? list)
         '()
         (select_nn 0 (min (length list) (length (car list))) list)
   )
)


(define (get i j matrix)
  (GetAtMatrix (GetAtMatrix matrix i) j)
)


(define (remove-at l i)
  (cond ( (null? l) l)
        ( (= i 0) (cdr l))
        (else (cons (car l) (remove-at (cdr l) (- i 1))))
   )
)


(define (remove-col l m)
  (map (lambda (x) (remove-at x m)) l)
)



(define (has? l1 l2)
  (if (null? l1)
         #f
         (or (member (car l1) l2)
             (has? (car l1) l2)
         )
  )
)


(define (f-help l suml)
  (if (null? l)
      #f
      (or (has? (remove-at suml (- (length suml) (length l))) l)
          (f-help (cdr l) suml)
       )
    )
)      

(define (fun? l)
  (if (null? l)
      #f
      (f-help l (map sum l))
  )
)

;TREES

(define (height t)
  (if (null? t) (cdr t)
              1
              (+ 1 (apply max (map height (cdr t))))
   )
)