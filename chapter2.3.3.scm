#lang scheme

(define (element-of-unordered-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-unordered-set? x (cdr set)))))

(define (adjoin-unordered-set? x set)
  (if (element-of-unordered-set? x set)
      set
      (cons x set)))

(define (intersection-unordered-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-unordered-set? (car set1) set2) (cons (car set1) (intersection-unordered-set (cdr set1) set2)))
        (else (intersection-unordered-set (cdr set1) set2))))

; exercise 2.59

(define (union-unordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-unordered-set? (car set1) set2) (union-unordered-set (cdr set1) set2))
        (else (union-unordered-set (cdr set1) (cons (car set1) set2)))))

; exercise 2.60

(define (adjoin-unordered-set2? x set)
      (cons x set))

(define (union-unordered-set2 set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (union-unordered-set2 (cdr set1) (cons (car set1) set2)))))

; exercise 2.61

(define (element-of-ordered-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-ordered-set? x (cdr set)))))

(define (intersection-ordered-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-ordered-set (cdr set1)
                                               (cdr set2))))
              ((< x1 x2)
               (intersection-ordered-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-ordered-set set1 (cdr set2)))))))

(define (adjoin-ordered-set x set)
  (define (add x set)
    (cond ((or (null? set) (< x (car set))) (cons x set))
          ((> x (car set)) (cons (car set) (add x (cdr set))))))
  (if (element-of-ordered-set? x set)
      set
      (add x set)))

; exercise 2.62

(define (union-ordered-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((equal? x1 x2) (cons x1 (union-ordered-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-ordered-set (cdr set1) set2)))
                 ((< x2 x1) (cons x2 (union-ordered-set set1 (cdr set2)))))))))

; sets as binary trees

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (if (null? set) 
      false
      (let ((node (entry set)))
        (cond ((equal? node x) true)
              ((> x node) (element-of-set? x (right-branch set)))
              ((< x node) (element-of-set? x (left-branch set)))))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set
                                      (adjoin-set x (right-branch set)))))))


(define (tree->list1 tree)
  (if (null? tree)
      '()
      (append (tree->list1 (left-branch tree))
              (cons (entry tree)
                    (tree->list1 (right-branch tree))))))

(define (tree->list2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; exercise 2.65

(define (union-set set1 set2)
  (let ((list1 (tree->list2 set1))
        (list2 (tree->list2 set2)))
    (list->tree (union-ordered-set list1 list2))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list2 set1))
        (list2 (tree->list2 set2)))
    (list->tree (intersection-ordered-set list1 list2))))   

; exercise 2.66

(define (key record) (car record))

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (entry set-of-records))) (entry set-of-records))
        ((< given-key (key (car set-of-records))) (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (car set-of-records))) (lookup given-key (right-branch set-of-records)))))