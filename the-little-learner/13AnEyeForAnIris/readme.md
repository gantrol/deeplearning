

## one-hot encoding

Why 1? Degree of belief



```scheme

```

## training

### weights problem

- symmetry problem
  - “Why can't we use zeros as we did before?”
- exploding: too large
- vanishing: too small

### l

1. random for breaking symmetry
2. exploding && vanishing -> gradient
3. how to fix it?

need weights to be:

- random
- not too large
- not too small

#### random-tensor

```scheme
; c v s -> tensor
; c: central value, number
; v: variance, number
; s: shape, (list ?)
(define random-tensor
  (lambda (c v s)
    ))
```

### dataset: training and test

- test set 20% general

## Model

an approximation of an idealized function represented by the data set
