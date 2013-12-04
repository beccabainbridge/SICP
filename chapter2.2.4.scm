#lang scheme

; procedures necessary for exercises

(define nil '())

(define (for-each f ls)
  (cond ((null? ls) (newline))
        (else (f (car ls))
              (for-each f (cdr ls)))))

(define (below bottom top)
  (list (list bottom) (list top)))

(define (beside left right)
  (cons left right))

(define (flip-vert x)
  (- x))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define flipped-pairs (square-of-four identity flip-vert identity flip-vert))

; exercise 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

; exercise 2.45

(define (split combine1 combine2)
  (define (x-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (x-split painter (- n 1))))
          (combine1 painter (combine2 smaller smaller)))))
  x-split)

(define right-split (split beside below))

; exercise 2.46

(define (make-vect x y)
  (cons x y))

(define xcor-vect car)
(define ycor-vect cdr)

(define (add-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (+ x1 x2) (+ y1 y2))))

(define (sub-vect v1 v2)
  (let ((x1 (xcor-vect v1))
        (y1 (ycor-vect v1))
        (x2 (xcor-vect v2))
        (y2 (ycor-vect v2)))
    (make-vect (- x1 x2) (- y1 y2))))

(define (scale-vect s v)
  (let ((x (xcor-vect v))
        (y (ycor-vect v)))
    (make-vect (* s x) (* s y))))

; exercise 2.47

(define (make-frame-list origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-list car)
(define edge1-frame-list cadr)
(define edge2-frame-list caddr)

 
(define origin-frame-cons car)
(define edge1-frame-cons cadr)
(define edge2-frame-cons cddr)

; exercise 2.48

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; exercise 2.49

(define (draw-line a b)
  (newline))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

(define painter-outline
  (let ((segment-list (list (make-segment (make-vect 0 0) (make-vect 1 0))
                            (make-segment (make-vect 0 0) (make-vect 0 1))
                            (make-segment (make-vect 1 1) (make-vect 1 0))
                            (make-segment (make-vect 1 1) (make-vect 0 1)))))
    (segments->painter segment-list)))

(define painter-x
  (let ((segment-list (list (make-segment (make-vect 0 0) (make-vect 1 1))
                            (make-segment (make-vect 1 0) (make-vect 0 1)))))
    (segments->painter segment-list)))


(define painter-diamond
  (let ((segment-list (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                            (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 0 0.5)))))
    (segments->painter segment-list)))