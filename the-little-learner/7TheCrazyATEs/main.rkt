#lang racket
(require malt)
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

(define lonely-i
  (λ (θ)
    (map (λ (p)
       (list p))
      θ)))

(define lonely-d
  (λ (big-theta)
    (map (λ (big-p)
       (ref big-p 0))
      big-theta)))

(define lonely-u
  (λ (big-theta gs)
    (map (λ (big-p grad)
       (list (- (ref big-p 0) (* alpha grad))))
      big-theta
      gs)))


; (define gradient-descent-v4
;   (λ (obj θ)
;     (let ((f (λ (big-theta)
;              (map (λ (item grad)
;                       (- item (* α grad)))
;                     big-theta
;                     (∇ obj big-theta)))))
;     (revise f revs θ))))
; (gradient-descent-v4
;   ((l2-loss line) line-xs line-ys)
;   (list 0.0 0.0))
(define gradient-descent-v5
  (λ (inflate deflate update)
    (λ (obj θ)
      (let ((f (λ (big-theta)
              (update
                big-theta
                (∇ obj (deflate big-theta))))))
      (deflate
        (revise f revs (inflate θ))))))
)

(define lonely-gradient-descent
  (gradient-descent-v5 lonely-i lonely-d lonely-u))
lonely-gradient-descent
(sampling-obj
  (l2-loss plane) plane-xs plane-ys)
; (lonely-gradient-descent
;           (sampling-obj
;             (l2-loss plane) plane-xs plane-ys)
;           (list [0 0] 0))


(define try-plane
  (lambda (a-gradient-descent)
    (with-hypers
      ([revs 15000]
       [alpha 0.001]
       [batch-size 4])
      (a-gradient-descent
          (sampling-obj
            (l2-loss plane) plane-xs plane-ys)
          (list (tensor 0 0) 0)))))


(try-plane lonely-gradient-descent)  ; same as `..\6AnAppleADay\main.rkt`

(define naked-i
  (λ (θ)
    (map (λ (p)
       (let ([big-p p])
            big-p))
      θ)))

(define naked-d
  (λ (big-theta)
    (map (λ (big-p)
       (let ([p big-p])
            p))
      big-theta)))

(define naked-u
  (λ (big-theta gs)
    (map (λ (big-p grad)
        (- big-p (* alpha grad)))
      big-theta
      gs)))

(define naked-gradient-descent
  (gradient-descent-v5 naked-i naked-d naked-u))

(try-plane naked-gradient-descent)

;; simplify ate funtions
(define gradient-descent-v6
  (λ (inflate deflate update)
    (λ (obj θ)
      (let ((f (λ (big-theta)
              (map update
                big-theta
                (∇ obj (map deflate big-theta))))))
      (map deflate
        (revise f revs 
          (map inflate θ)))))))

(define lonely-i-v6
  (λ (p)
    (list p)))

(define lonely-d-v6
  (λ (big-p)
    (ref big-p 0)))

(define lonely-u-v6
  (λ (big-p grad)
    (list (- (ref big-p 0) (* alpha grad)))))

(define lonely-gradient-descent-v6
  (gradient-descent-v6 lonely-i-v6 lonely-d-v6 lonely-u-v6))
lonely-gradient-descent-v6
(try-plane lonely-gradient-descent-v6)


(define naked-i-v6(λ (p)
  (let ([big-p p])
      big-p)))

(define naked-d-v6
  (λ (big-p)
    (let ([p big-p])
        p)))

(define naked-u-v6
  (λ (big-p grad)
        (- big-p (* alpha grad))))

(define naked-gradient-descent-v6
  (gradient-descent-v5 naked-i naked-d naked-u))

(try-plane naked-gradient-descent-v6)