

## Non-Linear f as a decider

> or activation f

```scheme
(define rectify-0
  (λ (s)
    (cond
     [(< s 0.0) 0.0]
     [else s])))

(define rectify
  (ext1 rectify-0 0))

; in: tensor
; out: (theta) => tensor
(define linear-1-1
  (λ (t)
    (λ (θ)
      (+ (dot-product (ref θ 0) t) (ref θ 1)))))

; in: tensor
; out: (theta) => tensor
(define relu-1-1
  (λ (t)
     (λ (θ)
        (rectify ((linear-1-1 t) θ)))))

(define half-strip
  (λ (x θ)
     (- [(relu-1-1 (tensor x)) (list (ref θ 0) (ref θ 1))]
        [(relu-1-1 (tensor x)) (list (ref θ 0) (ref θ 2))])))

(define full-strip
  (lambda (x theta)
    (- [(half-strip x) (list (ref theta 0) (ref theta 1) (ref theta 2))]
       [(half-strip x) (list (ref theta 3) (ref theta 4) (ref theta 5))])))
```

> relu: rectifying linear unit
>
> weighted decision about tensor `t`



