#lang scheme

(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? x) (symbol? x))

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))

; exercise 2.56 is below 2.57 and 2.58. Order swapped to aviod making duplicate deriv functions

;; exercise 2.57
;
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))
;
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))
;
;(define (sum? x)
;  (and (pair? x) (eq? (car x) '+)))
;
;(define (product? x)
;  (and (pair? x) (eq? (car x) '*)))
;
;(define (addend s) (cadr s))
;(define (augend s) 
;  (if (null? (cdddr s))
;      (caddr s)
;      (cons '+ (cddr s))))
;
;(define (multiplier p) (cadr p))
;(define (multiplicand p)
;  (if (null? (cdddr p))
;      (caddr p)
;      (cons '* (cddr p))))
;
;; exercise 2.58
;
;; a
;
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list a1 '+ a2))))
;
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list m1 '* m2))))
;
;(define (op x) (cadr x))
;
;(define (sum? x)
;  (and (pair? x) (eq? (op x) '+)))
;
;(define (product? x)
;  (and (pair? x) (eq? (op x) '*)))
;
;(define (addend s) (car s))
;(define (augend s) (caddr s))
;
;(define (multiplier p) (car p))
;(define (multiplicand p) (caddr p))

;b

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (op x) (cadr x))

(define (sum? x)
  (and (pair? x) (eq? (op x) '+)))

(define (product? x)
  (and (pair? x) (eq? (op x) '*)))

(define (addend s) (car s))
(define (augend s) 
  (if (or (pair? (caddr s)) (null? (cdddr s)))
      (caddr s)
      (cons (caddr s) (cdddr s))))

(define (multiplier p) (car p))
(define (multiplicand p)
  (if (or (pair? (caddr p)) (null? (cdddr p)))
      (caddr p)
      (cons (caddr p) (cdddr p))))

; exercise 2.56

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))
(define (exponent e) (caddr e))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                          (make-sum (exponent exp)
                                                                    (- 1))))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))
