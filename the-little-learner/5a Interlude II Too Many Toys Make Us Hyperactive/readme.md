# Hyperparameters

e.g. `α` and `revs` in later chapter.

- Should be select in problem after some thought and experimentation.
    - `α` and `revs` in later chapter may not work in other problem
- new construct

```
(declare-hyper smaller)
(declare-hyper larger)
```

no value

```
(with-hyper
  ((smaller 1)
    (larger 2000))
  (+ smaller larger))
```