#lang racket

;bst (x LT RT)
; (x '() '())




(define (key T)
  (car T)
)

(define (left T)
  (cadr T)
)

(define (right T)
  (caddr T)
 )

  

(define (search x t)
  (if (null? t)
        #f
        (or (= (key t) x)
            (if (< (key t) x)
                (search x (right t))
                (search x (left t))
            )
         )
  )
)

(define (add x T)
  (if (null? T)
      (make-leaf x)
      (if (> (key T) x) 
          (make-tree (key T) (add x (left T)) (right T))
          (make-tree (key T) (left T) (add x (right T)))
       )
   )
)


(define (get-max T)
  (if (null? T)
         T
         (if (null? (right T))
             (key T)
             (get-max (right T))
          )
   )
)

(define (remove-max T)
  (if (null? T)
          T
          (if (null? (right T))
              (left T)
              (make-tree (key T) (left T) (remove-max (right T)))
           )
   )
)

(define (height T)
  (if (null? T)
      0
      (+  1 (max (height (left T)) (height (right T))))
   )
)

(define (weight T)
  (if (null? T)
       0
       (+ 1 (weight (right T)) (weight (left T)))
   )
)



(define (balanced? T)
 (define (bal-help T f)
   (if (null? T)
        #t
        (and (bal-help (right T) f)
             (bal-help (left T) f)
             (< (abs (f (left T)) (f (right T))) 2)
         )
        
    )
  )
  (bal-help T height)
)
;ot prefixen izraz -> durvo
;fn deto go izchislqwa






(define (remove x T)
  (cond ((null? T) T)
        ((> (key T) (make-tree (key T) (remove x (left T)) (right T))))
        ((< (key T) (make-tree (key T)  (left T) (remove x (left T)))))
        ((and (null? (left T)) (null? (right T))) '())
        ((null? (left T)) (right T))
        ((null? (right T)) (left T))
        (else (make-tree (get-max (left T)) (remove-max (left T)) (right T)))
  )
)

(define (isLeaf T)
  (and (null? (left T))
       (null? (right T))
   )
)

(define (make-tree key l r)
  (list key l r)
)

(define (make-leaf key)
  (list key '() '())
)
  

(define (bloomingTree T)
  (if (isLeaf T)
      (make-tree (key T) (make-leaf (key T)) (make-leaf (key T)))
      (make-tree (key T) (bloomingTree (left T)) (bloomingTree (right T)))
  )
)






      
  