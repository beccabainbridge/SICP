#lang planet neil/sicp

; exercise 1.35

(define golden-ratio (/ (+ 1 (sqrt 5)) 2))
(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(close-enough? (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0) golden-ratio)

; exercise 1.36

(define (fixed-point-display f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display guess)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-display (lambda (x) (/ (log 1000) (log x))) 1.1)

; exercise 1.37

(define (cont-frac n d k)
  (define (iter i)
    (if (= i k)
        (/ (n i) (d i))
        (/ (n i) (+ (d i) (iter (+ i 1))))))
  (iter 1))

(define (cont-frac-iter n d k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1) (/ (n i) (+ (d i) result)))))
  (iter k 0))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           12)

; exercise 1.38

(define (eulers-expansion i)
  (if (= (remainder (- i 2) 3) 0)
      (* 2 (/ (+ i 1) 3))
      1))

(cont-frac (lambda (i) 1.0) eulers-expansion 11)

; exercise 1.39

(define (square x)
  (* x x))

; without cont-frac
(define (tan x k)
  (define (iter i)
    (if (>= i k)
        (/ (square x) (- (* i 2) 1))
        (/ (square x) (- (- (* i 2) 1) (iter (+ i 1))))))
  (if (> k 1)
      (/ x (- 1 (iter 2)))
      x))

; with cont-frac
(define (tan-cf x k)
  (cont-frac (lambda (i) 
                    (if (= i 1) 
                        x 
                        (- (square x)))) 
                  (lambda (i) 
                    (- (* i 2) 1)) 
                  k))

(tan 1.0 11)
(tan-cf 1.0 11)