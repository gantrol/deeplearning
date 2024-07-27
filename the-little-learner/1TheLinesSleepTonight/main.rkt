#lang racket
(require malt)
; it seems that some number unittest will error after require `malt`

(define line-v1 
  (lambda (x)
    (lambda (omiga beta)
      (let ((y (+ (* omiga x) beta)))
            y))))

(define line-xs
  (tensor 2.0 1.0 4.0 3.0))
(define line-ys
  (tensor 1.8 1.2 4.2 3.3))

(define line
  (lambda (x)
    (lambda (theta)
      (let ((y (+ (* (ref theta 0) x) (ref theta 1)))) y))))