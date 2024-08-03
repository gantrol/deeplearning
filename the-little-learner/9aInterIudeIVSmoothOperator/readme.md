# Smooth & decay rate

```
(define smooth
  (lambda (decay-rate average grad)
    (+ (* decay-rate average)
      (* (- 1 decay-rate) grad)))
)
```

## example1

- decay rate = 0.8
- history = 0
- input value list: [0, 5, 8, 3, 9, 7]
- output value list: [1, 2.4, 2.5, 3.8, 4.4]

contribution for the first item `5`:

1. 0.2 * 5
2. 0.8 * 0.2 * 5
3. 0.8 * 0.8 * 0.2 * 5
4. ...

## Example2

- decay rate = 0.9
- history = [0.8 3.1 2.2]
- input value list: [[1.0 1.1 3.0], [13.4 18.2 41.4], [1.1 0.3 67.3]]
- output value list: [[0.82 2.9 2.28], [2.08 4.43 6.19], [1.98 4.02 12.3]]