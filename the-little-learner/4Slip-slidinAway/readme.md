# Optimization by gradient descent

> chapter title: [Paul Simon | Slip Slidin' Away](https://www.youtube.com/watch?v=sz_5XJ4FccY)

frame 5
```
((l2-loss line) line-xs line-ys)
((l2-loss line) [2 1 4 3] [1.8 1.2 4.2 3.3])
(lambda (theta)
  (let ([pred-ys ((line [2 1 4 3]) theta)])
    (sum
     (sqr
      (- [1.8 1.2 4.2 3.3] pred-ys)))
    ))
```

6
```scheme
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
```

30
```scheme
(let ((α 0.01)
      (obj ((l2-loss line) line-xs line-ys)))
  (let ((f (λ (θ)
               (let ((gs (∇ obj θ)))
                 (list
                   W
                   B)))))
    (revise f 1000 (list 0.0 0.0))))
```
