#lang scheme

; exercise 1.16

(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

; helper functions for 1.17 and 1.18

(define (halve x)
  (/ x 2))

(define (double x)
  (* x 2))

; exercise 1.17

(define (multiply a b)
  (cond ((= b 0 ) 0)
        ((even? b) (double (multiply a (halve b))))
        (else (+ a (multiply a (- b 1))))))

; exercise 1.18

(define (multiply-iter a b total)
  (cond ((= b 0) total)
        ((even? b) (multiply-iter (double a) (halve b) total))
        (else (multiply-iter a (- b 1) (+ total a)))))

; exercies 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* q q) (* p p))
                   (+ (* q q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))
        