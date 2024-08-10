
- adaptive: change based on current and pass

> frame 34: This version of gradient-descent has the somewhat cryptic name RMSProp.† †Thanks, Geoffrey Everest Hinton.

```scheme
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
```
