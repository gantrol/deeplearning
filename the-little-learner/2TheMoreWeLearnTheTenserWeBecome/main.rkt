#lang racket
(require malt)
(define t1 (tensor 1 2 3 1))
(define t-embed (tensor (tensor (tensor 1))))

; scalar
(scalar? 3)
(scalar? 2.25)
(scalar? -9.52)
(scalar? t1)
; tensor
(tensor? (tensor 2 88 00 7))
(tensor? t-embed)
;; length
(tlen t1)
(tlen t-embed)
;; rank, how are its elements deeply nested. the number of brackets before the left most scalar
(rank t1)
(rank t-embed)

; (define rank-v0
;   (lambda (tensor)
;     (cond
;       [scalar? tensor 0]
;       [else (+ 1 (rank-v0 (tref tensor 0)))])))
; (rank-v0 t1)
; (rank-v0 t-embed)

;;; scalar is tensor^0

;; shape
(define shape
  (lambda (tensor)
    (cond
      [(scalar? tensor) (list )]
      [else (cons (tlen tensor) (shape (tref tensor 0)))]))
)
(shape t1)
(shape t-embed)