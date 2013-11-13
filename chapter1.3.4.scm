#lang planet neil/sicp

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cube x)
  (* x x x))

(define (square x)
  (* x x))

(define (inc x) (+ x 1))

(define (average a b)
  (/ (+ a b) 2))

; exercise 1.40

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

(define a 3)
(define b 6)
(define c 2)

(newtons-method (cubic a b c) 1)

; exercise 1.41

(define (double f)
  (lambda (x)
    (f (f x))))

(((double (double double)) inc) 5)

; exercise 1.42

(define (compose f g)
  (lambda (x)
    (f (g x))))

((compose square inc) 6)

; exercise 1.43

(define (repeated f n)
  (if (<= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

; exercise 1.44

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

; exercise 1.45

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (log2 x)
  (/ (log x) (log 2)))

(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (log2 x))) (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

; exercise 1.46

(define (iterative-improve improve good-enough)
  (define (iter x)
    (if (good-enough x)
        x
        (iter (improve x))))
  iter)

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve improve good-enough?) x))

(define (fixed-point2 f first-guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  (define (improve guess)
    (f guess))
  ((iterative-improve improve close-enough?) first-guess))