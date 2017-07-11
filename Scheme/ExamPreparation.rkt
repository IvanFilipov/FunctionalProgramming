#lang racket

;task 1
(define (meetCntTimes? f g a b cnt)

  (if (= cnt 0)
      #t
      (if (>= a b)
          #f
          (if (= (f a) (g a))
              (meetCntTimes? f g (+ 1 a) b (- cnt 1))
              (meetCntTimes? f g (+ 1 a) b cnt)
           )
       )
  )
)




(define (meetTwice? f g a b)
  (meetCntTimes? f g a b 2)
)

;task 2

(define MIN -100000)

(define (maxDuplicate list readed cur-Max)
  (if (null? list)
       cur-Max
       (if (and (member (car list) readed) (> (car list) cur-Max))
                (maxDuplicate (cdr list) readed (car list))
                  (maxDuplicate (cdr list) (cons (car list) readed) cur-Max)
        )          
  )
)

(define (maxDuplicates list)
  (if (null? list)
         MIN
         (max (maxDuplicate (car list) '() MIN) (maxDuplicates (cdr list)))
   )
 )
         
(define (maxDuplicate-ll list)
  (let ((res (maxDuplicates list)))
    (if (> res MIN)
        res
        #f
     )
   )
  )


;task3

(define (checkLine? line k)
  (if (null? line)
      #f
      (if (= (remainder (car line) k) 0)
          #t
          (checkLine? (cdr line) k)
       )
   )
 )

(define (checkMatrix? mat k)
  (if (null? mat)
      #t
      (and (checkLine? (car mat) k) (checkMatrix? (cdr mat) k))
   )
 )


;task4

(define (ls-Prefix list res)
  (if (null? list)
       (reverse res)
       (if (< (car list) (car res))
           (ls-Prefix (cdr list) (cons (car list) res))
           (reverse res)
        )
   )
)


(define (longer-str str1 str2)
  (if (< (length str1) (length str2))
      str2
      str1
      )
)

                            

(define (longestDescending l)
  (if (null? l )
      '()
      (longer-str (ls-Prefix (cdr l) (list (car l))) (longestDescending (cdr l)))
  )
)