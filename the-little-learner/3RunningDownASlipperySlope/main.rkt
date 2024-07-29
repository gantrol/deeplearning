#lang racket
(require malt)

; 1
(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))
;; start point
(define ys-v1 ((line line-xs) (list 0.0 0.0)))
(- ys-v1 line-ys)
;; loss
(define l2-loss-v1
  (lambda (xs ys)
    (lambda (theta)
      (let ([pred-ys ((line xs) theta)])
        (sum
          (sqr
            (- ys pred-ys)))))))
((l2-loss-v1 line-xs line-ys) (list 0.0 0.0))
;;; extract line
(define l2-loss-v2
  (lambda (line)
    (lambda (xs ys)
      (lambda (theta)
        (let ([pred-ys ((line xs) theta)])
          (sum
           (sqr
            (- ys pred-ys)))
          )))))
(((l2-loss-v2 line) line-xs line-ys) (list 0.0 0.0))
;;; rename line to target
(define l2-loss-v3
  (lambda (target)
    (lambda (xs ys)
      (lambda (theta)
        (let ([pred-ys ((target xs) theta)])
          (sum
           (sqr
            (- ys pred-ys)))
          )))))
(((l2-loss-v3 line) line-xs line-ys) (list 0.0 0.0))
;;; l2-loss in malt
(define l-v1 (((l2-loss line) line-xs line-ys) (list 0.0 0.0)))


; change theta and learning rate (lr)
;; change theta to find lr
(define theta-0-v2 0.0099)
(define ys-v2 ((line line-xs) (list theta-0-v2 0.0)))
(- ys-v2 line-ys)
(define l-v2 (((l2-loss line) line-xs line-ys) (list theta-0-v2 0.0)))
(define change-of-loss-v2 (- l-v2 l-v1))  ; why not 1 - 2?
(define rate-v2 (/ change-of-loss-v2 theta-0-v2))  ; rate
rate-v2
;; learning rate
(define lr 0.01)  ; alpha in this book
;;; new theta
(define theta-0-v3 (- 0 (* lr rate-v2)))
theta-0-v3
(define rate-v3 (((l2-loss line) line-xs line-ys)(list theta-0-v3 0)))
rate-v3
(define theta-0-v4 (- rate-v2 (* lr rate-v3)))
;;;; frame 44 cannot subtract again
theta-0-v4
(((l2-loss line) line-xs line-ys)(list theta-0-v4 0))

; end, next chapter will impl auto 0.0099
