#lang racket
(require malt)

; (define α 0.001)
; (define line-xs (tensor 2.0 1.0 4.0 3.0))
; (define line-ys (tensor 1.8 1.2 4.2 3.3))
(declare-hyper batch-size)

(define line-xs (tensor 2.0 1.0 4.0 3.0))
(define line-ys (tensor 1.8 1.2 4.2 3.3))

(define gradient-descent-v4
  (λ (obj θ)
    (let ((f (λ (big-theta)
             (map (λ (item grad)
                      (- item (* alpha grad)))
                    big-theta
                    (∇ obj big-theta)))))
    (revise f revs θ))))

(with-hypers
  ([revs 1000]
    [alpha 0.01])
  (gradient-descent-v4
  ((l2-loss line) line-xs line-ys)
  (list 0.0 0.0))
)

(define samples
  (λ (n s)
    (sampled n s (list))))

(define sampled
  (λ (n i a)
    (cond
      [(zero? i) a]
      [else
       (sampled n (sub1 i)
                (cons (random n) a))])))

(samples 20 3)


(define sampling-obj
  (lambda (expectant xs ys)
    (let ((n (tlen xs)))
      (lambda (theta)
        (let ((b (samples n batch-size)))
          ((expectant (trefs xs b) (trefs ys b)) theta))))))


(with-hypers
  ([revs 1000]
    [alpha 0.01]
    [batch-size 4])
  (gradient-descent-v4
    (sampling-obj
      (l2-loss line) line-xs line-ys)
    (list 0 0)))

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


(with-hypers
  ([revs 15000]
    [alpha 0.001]
    [batch-size 4])
  (gradient-descent-v4
    (sampling-obj
      (l2-loss plane) plane-xs plane-ys)
    (list (tensor 0 0) 0)))