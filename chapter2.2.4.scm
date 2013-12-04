#lang scheme

; procedures necessary for exercises

(define nil '())

(define (for-each f ls)
  (cond ((null? ls) (newline))
        (else (f (car ls))
              (for-each f (cdr ls)))))

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

; with list
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame car)
(define edge1-frame cadr)
(define edge2-frame caddr)

; with cons
(define (make-frame-cons origin edge1 edge2)
  (cons origin (cons edge1 edge2)))
 
(define origin-frame-cons car)
(define edge1-frame-cons cadr)
(define edge2-frame-cons cddr)

; exercise 2.48

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

; exercise 2.49

; created to stop not implemented error
(define (draw-line a b)
  (newline))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

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

; transform and combining painters

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter1
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

; exercise 2.50

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

; exercise 2.51

(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              (make-vect 1.0 0.0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.5)
                              (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

(define (below2 painter1 painter2)
  (let ((bottom (rotate90 painter1))
        (top (rotate90 painter2)))
    (rotate270 (beside top bottom))))

; exercies 2.52

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


; a
; modified diamond instead of wave
(define painter-diamond2
  (let ((segment-list (list (make-segment (make-vect 0 0.5) (make-vect 0.5 0))
                            (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                            (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                            (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                            (make-segment (make-vect 0.25 0.5) (make-vect 0.75 0.5)))))
    (segments->painter segment-list)))

; b

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner)))))

; c

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity flip-horiz)))
    (combine4 (corner-split painter n))))