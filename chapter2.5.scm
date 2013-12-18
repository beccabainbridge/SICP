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
  (let ((type-tags (map type-tag args)))
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
  (define (get-coercion-type poss-types type-tags)
    (cond ((null? poss-types) #f)
          ((can-coerce-all? (car poss-types) type-tags) (car poss-types))
          (else (get-coercion-type (cdr poss-types) type-tags))))
  (define (can-coerce-all? coercion-type types)
    (cond ((null? types) #t)
          ((get-coercion (car types) coercion-type) (can-coerce-all? coercion-type (cdr types)))
          (else #f)))
  (define (coerce-all coercion-type args)
    (if (null? args)
        '()
        (let ((arg (car args)))
          (cons ((get-coercion (type-tag arg) coercion-type) arg) (coerce-all coercion-type (cdr args))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coercion-type (get-coercion-type type-tags type-tags)))
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
  ;...
  (put 'raise 'complex
       (lambda (x) #f))
  'done)

(define (raise x) (apply-generic 'raise x))

; exercise 2.84

(define (apply-generic op . args)
  (define (try-raise a1 a2)
    (let ((new-a1 (raise a1)))
      (cond ((not new-a1) #f)
            ((eq? (type-tag new-a1) (type-tag a2)) new-a1)
            (else (try-raise new-a1 a2)))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "No method for this type"
                           (list op type-tags))
                    (let ((new-a1 (try-raise a1 a2))
                          (new-a2 (try-raise a2 a1)))
                      (cond (new-a1
                             (apply-generic op new-a1 a2))
                            (new-a2
                         (apply-generic op a1 new-a2))
                            (else
                             (error "No method for these types"
                                    (list op type-tags)))))))
              (error "No method for these types"
                     (list op type-tags)))))))

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

; need to replace +, -, * and / with add, sub, mul, and div in the complex package 
; and define cosine, sine, arctan, square, and squareroot in all other packages

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

; exercise 2.89

(define (adjoin-term-dense term term-list)
  (cons (coeff term) term-list))

(define (the-empty-list) '())
(define (first-term-dense term-list) (list (- (length term-list) 1) (car term-list)))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

; exercise 2.90

(define (install-polynomial-package)
  ;...
  (define (make-sparse var terms)
    ((get 'make-sparse 'sparse) var terms))
  (define (make-dense var terms)
    ((get 'make-dense 'dense) var terms))
  (put 'adjoin-term '(polynomial) adjoin-term)
  (put 'first-term '(polynomial) first-term)
  (put 'make-sparse 'polynomial
       (lambda (var term) (tag (make-sparse var term))))
  (put 'make-dense 'polynomial
       (lambda (var term) (tag (make-dense var term))))
  'done)

(define (install-sparse-package)
  ;...
  (define (adjoin-term term term-list)
    (if (=zero? term)
        term-list
        (cons (coeff term) term-list)))
  (define (first-term term-list) (car term-list))
  (define (sparse->dense terms)
    (define (iter terms result)
      (if (null? terms)
          result
          (let ((next-term (first-term term-list)))
            (if (eq? (order next-term) (length result))
                (iter (rest-terms terms) (cons (coeff next-term) result))
                (iter terms (cons 0 result))))))
    (iter terms the-empty-termlist))
  (define (make-sparse var terms)
    (cons var terms))
  (define (make-dense var terms)
    (cons var (sparse->dense terms)))
  (put 'adjoin-term '(sparse) adjoin-term)
  (put 'first-term '(sparse) first-term)
  (put 'make-sparse 'sparse 
       (lambda (var terms) (tag (make-sparse var terms))))
  (put 'make-dense 'sparse 
       (lambda (var terms) (tag (make-dense var terms))))
  'done)

(define (install-dense-package)
  ;...
  (define (adjoin-term term term-list)
    (cons (coeff term) term-list))
  (define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
  (define (dense->sparse terms)
    (if (null? terms)
        (the-empty-termlist)
        (let ((next-term (first-term terms)))
          (if (=zero? (coeff next-term))
              (dense->sparse (rest-terms terms))
              (cons (make-term (order next-term) (coeff next-term))
                    (dense->sparse (rest-terms terms)))))))
  (define (make-sparse var terms)
    (cons var (dense->sparse terms)))
  (define (make-dense var terms)
    (cons var terms))
  (put 'adjoin-term '(dense) adjoin-term)
  (put 'first-term '(dense) first-term)
  (put 'make-sparse 'dense 
       (lambda (var terms) (tag (make-sparse var terms))))
  (put 'make-dense 'dense
       (lambda (var terms) (tag (make-dense var terms))))
  'done)
  
; exercise 2.91

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
                     (div-terms (sub L1 
                                     (mul L2
                                          (list (make-term new-o new-c)))) 
                                L2)))
                (list (adjoin-term 
                       (make-term new-o new-c)
                       (car rest-of-result))
                      (cadr rest-of-result))))))))

