#lang racket
(require malt)

(declare-hyper revs)
(declare-hyper α)

;; quadratic data
(define quad-xs
  (vector -1 0 1 2 3))

(define quad-ys
  (vector 2.55 2.1 4.35 10.2 18.25))
;;; ax^2 + bx + c
(define quad
  (λ (t)
    (λ (θ)
      (+ (* (ref θ 0) (sqr t))
        (+ (* (ref θ 1) t) (ref θ 2))))))
(define θ-1 (list 4.5 2.1 7.8))
((quad 3) θ-1)

;;; objective function?
(define obj-quad ((l2-loss quad) quad-xs quad-ys))
(obj-quad θ-1)

(define gradient-descent-v4
  (λ (obj θ)
    (let ((f (λ (big-theta)
             (map (λ (item grad)
                      (- item (* α grad)))
                    big-theta
                    (∇ obj big-theta)))))
    (revise f revs θ))))

(with-hypers
  ([revs 1000]
   [α 0.001])
  (gradient-descent-v4
    obj-quad
    θ-1))
;; plane
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

(define plane
  (λ (t)
    (λ (θ)
      (+ (dot-product (ref θ 0) t) (ref θ 1)))))

; (define line
;   (λ (xs)
;     (λ (θ)
;       (+ (* (ref θ 0) xs) (ref θ 1)))))

(define dot-product-1-1
  (λ (w t)
    (sum-1
      (* w t))))
(dot-product-1-1 (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0))
(sum-1 (* (tensor 2.0 1.0 7.0) (tensor 8.0 4.0 3.0)))
(sum-1 (tensor 16 4 21))
line

((plane (tensor 2.0 3.91))
  (list (tensor 3.98 2.04) 5.78))
(dot-product (tensor 2.0 3.91) (tensor 3.98 2.04))