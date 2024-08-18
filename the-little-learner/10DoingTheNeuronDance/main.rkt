#lang racket
(require malt)


(define rectify-0
  (λ (s)
    (cond
     [(< s 0.0) 0.0]
     [else s])))
(define rectify
  (ext1 rectify-0 0))

(define linear-1-1
  (lambda (t)
    (lambda (theta)
      (+ (dot-product (ref theta 0) t) (ref theta 1)))))

(define relu-1-1
  (lambda (t)
    (lambda (theta)
      (rectify ((linear-1-1 t) theta)))))

(define half-strip
  (lambda (x theta)
    (- ((relu-1-1 x) (list (ref theta 0) (ref theta 1)))
       ((relu-1-1 x) (list (ref theta 0) (ref theta 2))))))

(define full-strip
  (lambda (x theta)
    (- [(half-strip x) (list (ref theta 0) (ref theta 1) (ref theta 2))]
       [(half-strip x) (list (ref theta 3) (ref theta 4) (ref theta 5))])))

(define full-plus-half-strip-example
  (λ (x)
    (+ (full-strip x
               (list (tensor 1) -1 -1.5
                     (tensor 1) -3 -3.5))
       (half-strip x
               (list (tensor 1) -1 -1.5)))))