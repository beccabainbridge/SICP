#lang scheme

(require "get-put.scm")

; exercise 2.78

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

; exercise 2.79

(define (install-scheme-number-package)
  ;...
  (put 'equ? '(scheme-number scheme-number) =)
  'done)

(define (install-rational-package)
  ;...
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  (put 'equ? '(rational rational) equ?)
  'done)

(define (install-complex-package)
  ;...
  (define (equ? x y)
    (and (= (real-part x) (real-part y)) (= (imag-part y) (imag-part x))))
  (put 'equ? '(complex complex) equ?1)
  'done)
  
(define (equ? x y) (apply-generic 'equ? x y))

; exercise 2.80

(define (install-scheme-number-package)
  ;...
  (put '=zero? 'scheme-number
       (lambda (x) (= x 0)))
  'done)

(define (install-rational-package)
  ;...
  (define (=zero? x)
    (= (numer x) 0))
  (put '=zero? 'rational =zero?)
  'done)

(define (install-complex-package)
  ;...
  (define (=zero? x y)
    (and (= (real-part x) 0) (= (imag-part x) 0)))
  (put '=zero? 'complex =zero?)
  'done)
  
(define (=zero? x y) (apply-generic '=zero? x y))


