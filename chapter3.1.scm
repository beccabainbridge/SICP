#lang scheme

; exercise 3.1
(define (make-accumulator value)
  (lambda (n)
    (set! value (+ n value))
    value))

; exercise 3.2
(define (make-monitored f)
  (let ((calls 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls) calls)
            ((eq? x 'reset-count) (set! calls 0))
            (else (set! calls (+ calls 1))
                  (f x))))))

; exercise 3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (lambda (x) "Incorrect password")))
  dispatch)

; exercise 3.4
(define (make-account2 balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (call-the-cops)
    "The cops have been called")
  (let ((attempts 0))
    (define (dispatch p m)
      (if (eq? p password)
          (cond ((eq? m 'withdraw) 
                 (begin (set! attempts 0)
                        withdraw))
                ((eq? m 'deposit) 
                 (begin (set! attempts 0)
                        deposit))
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m)))
          (lambda (x) 
            (if (>= attempts 7)
                (call-the-cops)
                (begin (set! attempts (+ attempts 1))
                       "Incorrect password")))))
    dispatch))

; exercise 3.5
(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (area x1 x2 y1 y2) (monte-carlo trials (points-test P x1 x2 y1 y2))))

(define (points-test P x1 x2 y1 y2)
  (lambda ()
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (P x y))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (area x1 x2 y1 y2)
  (* (- x2 x1) (- y2 y1)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (P x y)
  (<= (+ (expt (- x 5) 2)
         (expt (- y 7) 2))
      (expt 3 2)))

(define pi-estimation
  (/ (estimate-integral P 2 8 4 10 100)
     (expt 3.0 2)))
      
; exercise 3.6
(define random-init 1)
;placeholder for rand-update
(define (rand-update x)
  (+ x 1))
(define rand
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate) (set! x (rand-update x))
                               x)
            ((eq? m 'reset) (lambda (new-x)
                              (set! x new-x)))
            (else (error "Unknown request -- RAND"
                         m))))))

; exercise 3.7
(define (make-joint-account account old-password new-password)
  (define (dispatch p m)
    (if (eq? p new-password)
        (account old-password m)
        (lambda (x) "Incorrect password")))
  (if (eq? ((account old-password 'withdraw) 0) "Incorrect password")
      (error "Incorrect password")
      dispatch))

; exercise 3.8
(define f
  (let ((n #f))
    (lambda (x)
      (set! n (not n))
      (if n
          x
          0))))

(+ (f 0) (f 1))
(+ (f 1) (f 0))