
## Layer Function


```scheme
(λ (t)
   (λ (θ)
      ...tensor producing body...))
```

like target function, and can be.

t: input tensor

e.g. 7 length input tensor & 4 weight

```scheme
(λ (t)
  (λ (θ)
    (let ((w (ref θ 0)) (b (ref θ 1)))
      [(relu-1-1 t) (list (tref w 0) (tref b 0))]
      [(relu-1-1 t) (list (tref w 1) (tref b 1))]
      [(relu-1-1 t) (list (tref w 2) (tref b 2))]
      [(relu-1-1 t) (list (tref w 3) (tref b 3))])))
```

dense layer || fully-connected layers

- t: `(list n)`
  - θ
    - w: `(list m n)`
    - b: `(list m)`

### The Law of Dense Layers (Initial Version)

A dense layer function invokes *m* neurons on an *n* element input $tensor^1$ and produces an *m* element output tensor1.

### The Law of Dense Layers (Final Version)

```scheme
(λ (t)
  (λ (θ)
    (let ((w (ref θ 0)) (b (ref θ 1)))
      (*-2-1 w t))))
```



A dense layer function invokes *m* neurons on an *n*-element input tensor1 that produces an *m*-element output tensor1 in a single invocation of *2,1.

### linear



```scheme
(define dot-product-2-1
  (λ (w t)
     (sum
      (*-2-1 w t))))

; t: (list n) => { θ: (list (list m n) (list m)) => (list m) }
(define linear
  (λ (t)
     (λ (θ)
        (+ (dot-product-2-1 (ref θ 0) t) (ref θ 1)))))

(define relu
  (λ (t)
     (λ (θ)
        (rectify ((linear t) θ)))))

(define 1-relu
  (λ (t)
     (λ (θ)
        ((relu t) θ))))

(define 2-relu
  (λ (t)
     (λ (θ)
        ((relu
          ((relu t) θ))
         (refr θ 2)))))

(define 3-relu
  (λ (t)
     (λ (θ)
        ((2-relu
          ((relu t) θ))
         (refr θ 2)))))

(define k-relu
  (λ (count)
     (λ (t)
        (λ (θ)
           (cond
            [(= count 0) t]
            [else
             (((k-relu (- count 1)) ((relu t) θ)) (refr θ 2))])))))



```