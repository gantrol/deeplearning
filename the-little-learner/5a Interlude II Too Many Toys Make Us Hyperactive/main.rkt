#lang racket
(require malt)

(declare-hyper smaller)
(declare-hyper larger)

(with-hypers
  ((smaller 1)
    (larger 2000))
  (+ smaller larger))

(define nonsense?
  (λ (x)
    (= (sub1 x) smaller)))

(with-hypers
  ((smaller 5))
  (nonsense? 6))