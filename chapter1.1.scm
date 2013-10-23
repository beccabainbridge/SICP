#lang scheme

; functions for exercise 1.7

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

; altered to accept any function so can be used for square and cube
(define (good-enough? guess x f)
  (< (abs (- (f guess) x)) 0.001))

(define (good-enough2 guess last-guess)
  (< (/ (abs (- guess last-guess)) guess) 0.001))
  
(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (sqrt-iter guess)
    (if (good-enough? guess x square)
      guess
      (sqrt-iter (improve guess))))
  
  (sqrt-iter 1))

; sqrt-iter with better speed and accuracy for very large and very small numbers
(define (sqrt-iter2 guess last-guess x)
  (if (good-enough2 guess last-guess)
      guess
      (sqrt-iter2 (improve guess x)
                 guess x)))

; cube root functions for exercise 1.8
(define (cube x)
  (* x x x))

(define (my-cbrt x)
  (define (improve guess)
    (/ (+ (/ x (square guess)) 
	  (* 2 guess)) 
	3))
  
  (define (cbrt-iter guess)
    (if (good-enough? guess cube)
	guess
	(cbrt-iter (improve guess)
		  x)))

  (cbrt-iter 1))