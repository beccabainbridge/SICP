#lang scheme

(require "get-put.scm")

(define (=number? x n)
  (and (number? x) (= x n)))

(define (variable? x) (symbol? x))

(define (same-variable? x1 x2)
  (and (variable? x1) (variable? x2) (eq? x1 x2)))

; exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-deriv-package)
  ; sum
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (sum-deriv exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  ; product
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplier p) (car p))
  (define (multiplicand p) (cadr p))
  (define (product-deriv exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (multiplicand exp)
                   (deriv (multiplier exp) var))))
  (put 'deriv '+ sum-deriv)
  (put 'deriv '* product-deriv)
  'done)

; exercise 2.74

(define (get-record division employee)
  ((get division 'record) employee))

(define (get-salary division record)
  ((get division 'salary) record))

(define (find-employee-record employee divisions)
  (if (null? divisions)
      false
      (let ((record (get-record (car divisions) employee)))
        (if record
            record
            (find-employee-record employee (cdr divisions))))))

; exericse 2.75

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'magnitiude) r)
          ((eq? op 'angle) a)
          ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'real-part) (* r (sin a)))
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)