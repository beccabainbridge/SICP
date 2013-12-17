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

; exercise 2.81

(define (apply-generic op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for this type"
                           (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

; exercise 2.82

(define (apply-generic-mult op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion-type (get-coercion-type type-tags)))
            (if coercion-type
                (apply apply-generic
                       (append (list op) (coerce-all coercion-type args)))
                (error "No method for these types"
                     (list op type-tags))))))))

; exercise 2.83


(define (install-scheme-number-package)
  ;...
  (put 'raise 'scheme-number
       (lambda (x) ((get 'make 'rational) x 1)))
  'done)

(define (install-rational-package)
  ;...
  (put 'raise 'rational
       (lambda (x) ((get 'make 'real) (/ (numer x) (denom x)))))
  'done)

(define (install-real-package)
  ;...
  (put 'raise 'real
       (lambda (x) ((get 'make-from-real-imag 'complex) x 0)))
  'done)

(define (install-complex-package)
  ;..
  'done)

(define (raise x) (apply-generic 'raise x))

; exercise 2.84

(define (apply-generic-mult op . args)
  (let ((type-tags (map type-tags args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion-type (get-coercion-type type-tags)))
            (if coercion-type
                (apply apply-generic
                       (append (list op) (coerce-all coercion-type args)))
                (error "No method for these types"
                       (list op type-tags))))))))

; exercise 2.85

(define (install-scheme-number-package)
  ;...
  'done)

(define (install-rational-package)
  ;...
  (put 'project 'rational
       (lambda (x) ((get 'make 'scheme-number) (floor (/ (numer x) (denom x))))))
  'done)

(define (install-real-package)
  ;...
  (put 'project 'real
       (lambda (x) ((get 'make 'rational) x 1)))
  'done)

(define (install-complex-package)
  ;...
  (put 'project 'complex
       (lambda (x) ((get 'make 'real) (real-part x))))
  'done)

(define (project x) (apply-generic 'project x))

(define (drop x)
  (let ((drop-raise (raise (drop x))))
    (if (eq? drop-raise x)
        (drop x)
        x)))

; exercise 2.86


; exercise 2.87

(define (install-polynomial-package)
  ;...
  (define (=zero? x)
    (empty-termlist? (termlist x)))
  (put '=zero? 'polynomial =zero?)
  'done)

; exercise 2.88

(define (sub-poly p1 p2)
  (if (same-variable (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (negate-terms (term-list p2))))
      (error "Polys not in same var -- SUB-POLY"
             (list p1 p2))))

(define (negate-terms term-list)
  (if (empty-termlist? term-list)
      (the-empty-termlist)
      (let ((next-term (first-term term-list)))
        (adjoin-term (make-term (order next-term) (neg (coeff next-term)))
                     (negate-terms (rest-terms term-list))))))