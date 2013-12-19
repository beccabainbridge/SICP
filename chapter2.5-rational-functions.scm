#lang scheme

(require "get-put.scm")

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

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "NO method for these types -- APPLY-GENERIC"
                 (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (neg x) (apply-generic 'neg x))
(define (=zero? x) (apply-generic '=zero? x))
(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))
(define (reduce a b) (apply-generic 'reduce a b))

(define (install-scheme-number-package)
  (define (reduce-integers n d)
    (let ((g (gcd n d)))
      (list (/ n g) (/ d g))))
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'neg '(scheme-number)
       (lambda (x) (tag (- x))))
  (put '=zero? '(scheme-number)
       (lambda (x) (= x 0)))
  (put 'greatest-common-divisor '(scheme-number scheme-number)
       (lambda (a b) (tag (gcd a b))))
  (put 'reduce '(scheme-number scheme-number)
       (lambda (n d) (reduce-integers n d)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (reduce n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  (define (neg-rat x)
    (make-rat (neg (numer x))
              (denom x)))
  (define (=zero? x) (= (numer x) 0))
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'neg 'rational
       (lambda (x) (tag (neg-rat x))))
  (put '=zero? '(rational) 
       (lambda (x) (=zero? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-polynomial-package)
  ;; internal procedures
  ;; represetation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;; variable checks
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x) (symbol? x))
  ;; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  ;; ploy math
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY"
               (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (sub-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
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
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((result (div-terms (term-list p1)
                                 (term-list p2))))
          (list (make-poly (variable p1)
                           (car result))
                (make-poly (variable p1)
                           (cadr result))))
        (error "Polys not in same var -- DIV-POLY"
               (list p1 p2))))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (add-terms L1 
                                       (negate-terms (mul-term-by-all-terms
                                                      (make-term new-o new-c)
                                                      L2))) 
                                  L2)))
                  (list (adjoin-term 
                         (make-term new-o new-c)
                         (car rest-of-result))
                        (cadr rest-of-result))))))))
  (define (zero-poly? x)
    (empty-termlist? (term-list x)))
  ;; from exercise 2.94
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- GCD-POLY"
               (list p1 p2))))
  (define (gcd-terms a b)
    (if (empty-termlist? b)
        ; from exercise 2.96b
        (let ((coeffs (map coeff a)))
          (let ((coeffs-gcd (apply gcd coeffs)))
            (map (lambda (x) 
                   (make-term (order x) 
                              (div (coeff x) coeffs-gcd)))
                 a)))
        (gcd-terms b (pseudoremainder-terms a b))))
  (define (quotient-terms a b)
    (car (div-terms a b)))
  (define (remainder-terms a b)
    (cadr (div-terms a b)))
  ; from exercise 2.96a
  (define (pseudoremainder-terms p q)
    (let ((t1 (first-term p)) (t2 (first-term q)))
      (let ((o1 (order t1))
            (o2 (order t2))
            (c (coeff t2)))
        (cadr (div-terms
               (mul-term-by-all-terms (make-term 0
                                                 (expt c (+ 1 o1 (- o2))))
                                      p)
               q)))))
  ; from exercise 2.97a
  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((reduced (reduce-terms (term-list p1)
                                      (term-list p2))))
          (list (make-poly (variable p1)
                           (car reduced))
                (make-poly (variable p2)
                           (cadr reduced))))
        (error "Polys not in same var -- REDUCE-POLY"
               (list p1 p2))))
  (define (reduce-terms L1 L2)
    (let ((g (gcd-terms L1 L2)))
      (let ((g1 (first-term g)))
        (let ((c (coeff g1))
              (o1 (max (order (first-term L1))
                       (order (first-term L2))))
              (o2 (order g1)))
          (let ((n (mul-term-by-all-terms (make-term 0
                                                 (expt c (+ 1 o1 (- o2))))
                                           L1))
                (d (mul-term-by-all-terms (make-term 0
                                                 (expt c (+ 1 o1 (- o2))))
                                           L2)))
                (list (quotient-terms n g) (quotient-terms d g)))))))
  ;; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)
         (let ((result (div-poly p1 p2)))
           (if (=zero? (tag (cadr result)))
               (tag (car result))
               (list (tag (car result))
                     (tag (cadr result)))))))
  (put '=zero? '(polynomial) 
       (lambda (x) (zero-poly? x)))
  (put 'greatest-common-divisor '(polynomial polynomial)
       (lambda (p1 p2) (tag (gcd-poly p1 p2))))
  (put 'reduce '(polynomial polynomial)
       (lambda (p1 p2) 
         (let ((result (reduce-poly p1 p2)))
         (list (tag (car result))
               (tag (cadr result))))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (make-polynomial variable termlist)
  ((get 'make 'polynomial) variable termlist))

(install-scheme-number-package)
(install-rational-package)
(install-polynomial-package)

; exercise 2.93

; changes to rational package made above

(define p1 (make-polynomial 'x '((2 1) (0 1))))
(define p2 (make-polynomial 'x '((3 1) (0 1))))
(define rf (make-rational p2 p1))
;(add rf rf)
; '(rational (polynomial x (5 2) (3 2) (2 2) (0 2)) polynomial x (4 1) (2 2) (0 1))

; exercise 2.94

; changes to polynomial package made above

(define p3 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
(define p4 (make-polynomial 'x '((3 1) (1 -1))))
(greatest-common-divisor p3 p4)
; '(polynomial x (2 -1) (1 1))

; exercise 2.95

(define p5 (make-polynomial 'x '((2 1) (1 -2) (0 1))))
(define p6 (make-polynomial 'x '((2 11) (0 7))))
(define p7 (make-polynomial 'x '((1 13) (0 5))))

(define q1 (mul p5 p6))
(define q2 (mul p5 p7))

(greatest-common-divisor q1 q2)
; with remainder-terms
; '(polynomial x (2 1458/169) (1 -2916/169) (0 1458/169))
; with psuedoremaider-terms
; '(polynomial x (2 1458) (1 -2916) (0 1458))

; exercise 2.97

(define p8 (make-polynomial 'x '((1 1) (0 1))))
(define p9 (make-polynomial 'x '((3 1) (0 -1))))
(define p10 (make-polynomial 'x '((1 1))))
(define p11 (make-polynomial 'x '((2 1) (0 -1))))

(define rf1 (make-rational p8 p9))
(define rf2 (make-rational p10 p11))
(add rf1 rf2)
; '(rational (polynomial x (3 -1) (2 -2) (1 -3) (0 -1))
;            (polynomial x (4 -1) (3 -1) (1 1) (0 1)))



