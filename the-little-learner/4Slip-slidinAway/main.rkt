#lang racket
(require malt)
;((l2-loss line) line-xs line-ys)
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