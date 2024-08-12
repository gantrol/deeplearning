# Extend function

base function

## ext1

23

```scheme
(define ext1
  (lambda (f n)
    (lambda (t)
      (cond
       [(of-rank? n t) (f t)]
       [else (tmap (ext1 f n) t)]))))

(define of-rank?
  (lambda (n t)
    (cond
     [(zero? n) (scalar? t)]
     [(scalar? t) #f]
     [else (of-rank? (sub1 n) (tref t 0))]
     )))
```

## ex2

35

```scheme
(define ex2
  (lambda (f n m)
    (lambda (t u)
      (cond
       [(of-ranks n t m u) (f t u)]
       [else (desc (ex2 f n m) n t m u)]
       ))))



```

进度在41frame