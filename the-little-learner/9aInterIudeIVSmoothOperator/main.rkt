#lang racket
(require malt)
(define smooth
  (lambda (decay-rate average grad)
    (+ (* decay-rate average)
      (* (- 1 decay-rate) grad)))
)

(smooth 0.9 0.0 50.3)
(smooth 0.9 5.03 22.7)
(smooth 0.9 6.8 4.3)


(define smooth-list
  (lambda (decay-rate input-list)
    (if (null? input-list)
        '()
        (let loop ((input-list (cdr input-list))
                   (average (car input-list))
                   (output-list (list (car input-list))))
          (if (null? input-list)
              (reverse output-list)
              (let* ((grad (car input-list))
                     (new-average (smooth decay-rate average grad)))
                (loop (cdr input-list)
                      new-average
                      (cons new-average output-list))))))))

;; Example 1
(define decay-rate 0.8)
(define input-values '(0 5 8 3 9 7))
(define output-values (smooth-list decay-rate input-values))
output-values

;; Example 2
; (define decay-rate-2 0.9)
; (define input-values-2 '((tensor 1.0 1.1 3.0) (tensor 13.4 18.2 41.4) (tensor 1.1 0.3 67.3)))
; (define output-values-2 (smooth-list decay-rate-2 input-values-2))
; (display output-values-2)