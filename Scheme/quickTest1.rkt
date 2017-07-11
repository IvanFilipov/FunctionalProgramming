#lang racket


(define g +)

(define (f x y) (+ x y))

(define a 5)



(let ((d (f a a)))(let ((b (+ a d)))(let ((c (g b d))) (+ b c d))))