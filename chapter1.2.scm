#lang scheme

; exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; exercise 1.11

; recursive process

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) 
         (* (f (- n 2)) 2) 
         (* (f (- n 3)) 3))
      ))

; iterative process
(define (f2 n)
  (define (f-iter x1 x2 x3 count)
    (if (> count n)
        x1
        (f-iter (+ x1 (* 2 x2) (* 3 x3)) x1 x2 (+ count 1))))
  
  (if (< n 3)
      n
      (f-iter 2 1 0 3)))

; exercise 1.12

(define (p-tri row col)
  (cond ((or (< col 1) (> col row)) 0)
        ((or (= col 1) (= col row)) 1)
        (else (+ (p-tri (- row 1) (- col 1)) 
                 (p-tri (- row 1) col)))))