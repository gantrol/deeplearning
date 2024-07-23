#lang racket
(require rackunit)

; define variables
(define pie 3.14)
(define a-radius 8.4)

; define function
(define area-of-circle
  (lambda (radius)
    (* pie 
      (* radius radius))))
(check-equal? (area-of-circle a-radius) 221.5584)
;; function returns function
(define area-of-rectangle
  (lambda (width)
    (lambda (height)
      (* width height))))
(check-pred procedure? (area-of-rectangle 3.5))
;; function uses function input
(define call-twice
  (lambda (f)
    (lambda (x)
      (f (f x)))))
(check-pred procedure? (call-twice (lambda (x) (* x x))))
(check-equal? ((call-twice (lambda (x) (* x x))) 4) 256)

; condition
(check-equal?
  (cond
    [(= pie 4) 28]
    [(< pie 4) 42]
    (else 88))
  42
  )
;; cond in function
(define abs
  (lambda (x)
    (cond
      [(< x 0) (- 0 x)]
      [else x])))
(check-equal? 1 (abs 1))
(check-equal? 5 (abs -5))

; let
(define silly-abs
  (lambda (x)
    (let ((x-is-negative (< x 0)))
      (cond
        (x-is-negative (- 0 x))
        (else x)))))

; recursive function: without loops
(define remainder
  (lambda (x y)
    (cond
      [(< x y) x]
      [else (remainder (- x y) y)])))

(check-equal? (remainder 13 4) 1)