#lang planet neil/sicp

(define (square n)
  (* n n))

; exercise 1.21

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(display (smallest-divisor 199))
(display "\n")
(display (smallest-divisor 1999))
(display "\n")
(display (smallest-divisor 19999))

; exercise 1.22

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start numprimes)
  (cond ((= numprimes 0) (newline))
        ((prime? start)
         (timed-prime-test start)
         (search-for-primes (+ start 2) (- numprimes 1)))
        (else (search-for-primes (+ start 2) numprimes))))

(search-for-primes 1001 3)
(search-for-primes 10001 3)
(search-for-primes 100001 3)
(search-for-primes 1000001 3)

; exercise 1.23
 
; needs to be moved above find-divisor to run
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

; output with next
; 1009 *** 26
; 1013 *** 5
; 1019 *** 5
;
; 10007 *** 12
; 10009 *** 12
; 10037 *** 12
;
; 100003 *** 34
; 100019 *** 34
; 100043 *** 34
;
; 1000003 *** 105
; 1000033 *** 105
; 1000037 *** 106

; output without next
; 1009 *** 28
; 1013 *** 6
; 1019 *** 6
;
; 10007 *** 16
; 10009 *** 16
; 10037 *** 17
;
; 100003 *** 48
; 100019 *** 60
; 100043 *** 50
;
; 1000003 *** 158
; 1000033 *** 159
; 1000037 *** 153

; exercise 1.24

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
  
(define (fast-timed-prime-test n)
  (newline)
  (display n)
  (fast-start-prime-test n (runtime)))

(define (fast-start-prime-test n start-time)
  (if (fast-prime? n 5)
      (report-prime (- (runtime) start-time))))

(define (fast-search-for-primes start numprimes)
  (cond ((= numprimes 0) (newline))
        ((fast-prime? start 5)
         (fast-timed-prime-test start)
         (fast-search-for-primes (+ start 2) (- numprimes 1)))
        (else (fast-search-for-primes (+ start 2) numprimes))))

(fast-search-for-primes 1001 3)
(fast-search-for-primes 10001 3)
(fast-search-for-primes 100001 3)
(fast-search-for-primes 1000001 3)

; exercise 1.25

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt  b (- n 1))))))

(define (simple-expmod base exp m)
  (remainder (fast-expt base exp) m))

(simple-expmod 2 7 17)
(expmod 2 7 17)

; exercise 1.27

(define (all-fermat-tests n)
  (define (test count)
    (if (= count n)
        true
        (and (expmod count n n) (test (+ count 1)))))
  (test 1))

(all-fermat-tests 561)
(all-fermat-tests 1105)
(all-fermat-tests 1729)
(all-fermat-tests 2465)
(all-fermat-tests 2821)
(all-fermat-tests 6601)

; exercise 1.28

(define (mr-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (mr-expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (mr-expmod base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (mr-expmod a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))