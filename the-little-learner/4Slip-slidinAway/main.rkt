#lang racket
(require malt)

(define obj
  (lambda (theta)
    (let ([pred-ys ((line (vector 2 1 4 3)) theta)])
      (sum
       (sqr
        (- (vector 1.8 1.2 4.2 3.3) pred-ys)))
      )))
(obj (list -1.0 0.0))
(obj (list 0.0 0.0))
(obj (list 1.0 0.0))
(obj (list 2.0 0.0))
(obj (list 3.0 0.0))

;; nabla ∇
(∇ obj (list 0.0 0.0))

;; auto revise
(define revise
  (λ (f revs θ)
    (cond
      [(zero? revs) θ]
      [else (revise f (sub1 revs) (f θ))])))

;; map
(define f-1 (λ (θ)
              (map (λ (p)
                     (- p 3))
                   θ)))
(revise f-1 5 (list 1 2 3))

(map (λ (x y)
       (+ x y))
     (list 12 17 32)
     (list 8 3 11))

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

;; gradient descent
;;; v1
(let ((alpha 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (λ (θ)
             (let ((gs (∇ obj θ)))
               (list
                (- (ref θ 0) (* alpha (ref gs 0)))
                (- (ref θ 1) (* alpha (ref gs 1)))
                )))))
    (revise f 1000 (list 0.0 0.0))))
;;; v2
(let ((alpha 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (λ (θ)
             (let ((gs (∇ obj θ)))
               (map (λ (item grad)
                      (- item (* alpha grad)))
                    θ
                    gs)))))
    (revise f 1000 (list 0.0 0.0))))
;;; v3
(define revs 1000)
(define alpha 0.01)

(let ((obj ((l2-loss line) line-xs line-ys)))
  (let ((f (λ (θ)
             (let ((gs (∇ obj θ)))
               (map (λ (item grad)
                      (- item (* alpha grad)))
                    θ
                    gs)))))
    (revise f revs (list 0.0 0.0))))
;;; v4
(define gradient-descent-v4
  (λ (obj θ)
    (let ((f (λ (big-theta)
             (map (λ (item grad)
                      (- item (* alpha grad)))
                    big-theta
                    (∇ obj big-theta)))))
    (revise f revs θ))))
(gradient-descent-v4
  ((l2-loss line) line-xs line-ys)
  (list 0.0 0.0))
