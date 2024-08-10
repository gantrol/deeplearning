#lang racket
(require malt)

(declare-hyper beta)
(define epsilon 1e-08)

(define smooth
  (lambda (decay-rate average grad)
    (+ (* decay-rate average)
      (* (- 1 decay-rate) grad)))
)
;; rms
(define rms-u-mine
  (lambda (big-p grad)
    (let ([r (smooth beta (ref big-p 1) (abs grad))])
      (let ([alpha-hat (/ alpha (+ r epsilon))])
        (list (- (ref big-p 0) (* alpha-hat grad)) r)))))

(define rms-u
  (lambda (big-p grad)
    (let ([r (smooth beta (ref big-p 1) (sqr grad))])
      (let ([alpha-hat (/ alpha (+ (sqrt r)  epsilon))])
        (list (- (ref big-p 0) (* alpha-hat grad)) r)))))

(define rms-i
  (lambda (p)
    (list p (zeroes p))))

(define rms-d 
  (lambda (big-p)
    (ref big-p 0)))

(define rms-gradient-descent
  (gradient-descent rms-i rms-d rms-u))
(define rms-gradient-descent-mine
  (gradient-descent rms-i rms-d rms-u-mine))
;;; plane
(define plane-xs
  (tensor
    (tensor 1.0 2.05)
    (tensor 1.0 3.0)
    (tensor 2.0 2.0)
    (tensor 2.0 3.91)
    (tensor 3.0 6.13)
    (tensor 4.0 8.09)))

(define plane-ys
  (tensor
    13.99
    15.99
    18.0
    22.4
    30.2
    37.94))
(define try-plane-v2
  (lambda (a-gradient-descent a-revs an-alpha)
    (with-hypers
      ([revs a-revs]
       [alpha an-alpha]
       [batch-size 4])
      (a-gradient-descent
        (sampling-obj
          (l2-loss plane) plane-xs plane-ys)
        (list (tensor 0 0) 0)))
  ))

(with-hypers
  ([beta 0.9])
  (try-plane-v2
    rms-gradient-descent 3000 0.01))

(with-hypers
  ([beta 0.9])
  (try-plane-v2
    rms-gradient-descent-mine 3000 0.01))

;; adam
;;; big-p is (list p v r)
(define adam-u
  (lambda (big-p grad)
    (let ([r (smooth beta (ref big-p 2) (sqr grad))])
      (let ([alpha-hat (/ alpha (+ (sqrt r) epsilon))]
            [v (smooth mu (ref big-p 1) grad)])
        (list (- (ref big-p 0) (* alpha-hat v)) v r)))))

(define adam-i
  (lambda (p)
    (let ([v (zeroes p)])
      (let ([r v])
        (list p v r)))))

(define adam-d
  (lambda (big-p)
    (ref big-p 0)))

(define adam-gradient-descent
  (gradient-descent adam-i adam-d adam-u))

(with-hypers
  ([beta 0.9]
   [mu 0.85])
  (try-plane-v2
    adam-gradient-descent 1500 0.01))  ;;;? could not get the result with alpha 0.001 in the book







