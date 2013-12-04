#lang scheme

(define nil '())

(define (square x)
  (* x x))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

; exercise 2.17

(define (last-pair ls)
  (if (null? (cdr ls))
        (list (car ls))
        (last-pair (cdr ls))))

; exercise 2.18

(define (reverse list)
  (define (iter ls rs)
    (if (null? ls)
        rs
        (iter (cdr ls) (cons (car ls) rs))))
  (iter list '()))

; exercise 2.19

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 4 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (null? coins))

; exercise 2.20

(define (same-parity . ns)
    (define (iter compare rest out)
      (cond ((null? rest) out)
            ((compare (car rest)) (iter compare (cdr rest) (append out (list (car rest)))))
            (else (iter compare (cdr rest) out))))
    (if (even? (car ns))
        (iter even? ns '())
        (iter odd? ns '())))

; exercise 2.21

(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-map items)
  (map square items))

; exercise 2.23

(define (for-each f ls)
  (cond ((null? ls) #t)
        (else (f (car ls))
              (for-each f (cdr ls)))))

; exercise 2.25

(define ls1 (list 1 3 (list 5 7)))

(car (cdr (car (cdr (cdr ls1)))))

(define ls2 (list (list 7)))

(car (car ls2))

(define ls3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ls3))))))))))))

; exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
;(list 1 2 3 4 5 6)
(cons x y)
;(list (list 1 2 3) 4 5 6)
(list x y)
;(list (list 1 2 3) (list 4 5 6))

; exercise 2.27

(define (deep-reverse list)
  (define (rev ls rs)
    (cond ((null? ls) rs)
          ((pair? (car ls)) (rev (cdr ls) (cons (rev (car ls) '()) rs)))
          (else (rev (cdr ls) (cons (car ls) rs)))))
  (rev list '()))

(define (deep-reverse2 ls)
  (if (pair? ls)
      (append (deep-reverse2 (cdr ls)) (list (deep-reverse2 (car ls))))
      ls))

; exercise 2.28

(define (fringe tree)
  (cond ((null? tree) tree)
        ((pair? tree) (append (fringe (car tree)) (fringe (cdr tree))))
        (else (list tree))))

; exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile) (car mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length branch) (car branch))
(define (branch-structure branch) (car (cdr branch)))

; b
(define (branch-weight branch)
    (if (not (pair? (branch-structure branch)))
        (branch-structure branch)
        (total-weight (branch-structure branch))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

; c
(define (branch-torque branch)
  (if (not (pair? (branch-structure branch)))
      (* (branch-length branch) (branch-structure branch))
      (balanced? (branch-structure branch))))

(define (balanced? mobile)
  (equal? (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile))))

; d
; constructors using cons instead of list
;(define (make-mobile left right)
;  (cons left right))
;
;(define (make-branch length structure)
;  (cons length structure))
;
;(define (left-branch mobile) (car mobile))
;(define (right-branch mobile) (cdr mobile))
;
;(define (branch-length branch) (car branch))
;(define (branch-structure branch) (cdr branch))

; exercise 2.30

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((pair? tree) (cons (square-tree (car tree))
                            (square-tree (cdr tree))))
        (else (square tree))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree))

; exercise 2.31

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map f sub-tree)
             (f sub-tree)))
       tree))

; exercise 2.32

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (ls) (cons (car s) ls)) rest)))))

; exercise 2.33
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

; exercise 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* higher-terms x)))
              0
              coefficient-sequence))

; exercise 2.35

(define (count-leaves t)
  (accumulate (lambda (x y) (+ y 1)) 0 (map (lambda (x) x) (fringe t))))

; exercise 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (s) (car s)) seqs))
            (accumulate-n op init (map (lambda (s) (cdr s)) seqs)))))


; exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

; exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3))
; 3/2
(fold-left / 1 (list 1 2 3))
; 1/6
(fold-right list nil (list 1 2 3))
; (1 (2 (3 nil)))
(fold-left list nil (list 1 2 3))
; (((nil 1) 2) 3)

; exercise 2.39

(define (foldr-reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (foldl-reverse sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))

; exercies 2.40

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime? n)
  (define (find-divisor test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (find-divisor 2)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

; exercise 2.41

(define (distinct-triple-sum n s)
  (filter (lambda (pair)
            (= (+ (car pair) (cadr pair) (caddr pair)) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k)
                                       (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

; exercies 2.42

(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))

(define empty-board nil)

(define (safe? k positions)
  (let ((new-col (car positions)))
    (define (clear-row col)
      (not (= (car new-col) (car col))))
    (define (clear-diag col)
      (and (not (= (+ (car new-col) (cdr new-col)) 
                   (+ (car col) (cdr col))))
           (not (= (- (- (car new-col) (car col))
                      (- (cdr new-col) (cdr col)))
                 0))))
    (define (clear cols)
      (cond ((null? cols) #t)
              ((not (and (clear-row (car cols)) 
                         (clear-diag (car cols)))) 
               #f)
              (else (clear (cdr cols)))))
    (clear (cdr positions))))
      


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
         
(define (display-board board)
  (let ((length (acc-length board)))
    (define (create-row col k)
      (cond ((= k 0) nil)
            ((= k col) (cons 1 (create-row col (- k 1))))
            (else (cons 0 (create-row col (- k 1))))))
    (define (display-rows rows)
      (cond ((null? rows) (newline))
          (else (display (create-row (caar rows) length))
                 (newline)
                 (display-rows (cdr rows)))))
    (display-rows board)))

(define (display-boards boards)
  (cond ((null? boards) (newline))
      (else (display-board (car boards))
            (display-boards (cdr boards)))))

