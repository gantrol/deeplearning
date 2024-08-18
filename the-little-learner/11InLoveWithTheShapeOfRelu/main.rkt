#lang racket
(require malt)


(define w1 (tensor (tensor 2.0 1.0 3.1) (tensor 3.7 4.0 6.1)))
(define t1 (tensor 1.3 0.4 3.3))
(define b1 (tensor -100 5))

(define w2 (tensor (tensor 2.0 1.0) (tensor 3.7 4.0)))
(define b2 (tensor 100 5))

(define theta1 (list w1 b1))

(define dot-product-2-1
  (λ (w t)
     (sum
      (*-2-1 w t))))
(dot-product-2-1 w1 t1)


; t: (list n) => { θ: (list (list m n) (list m)) => (list m) }
(define linear
  (λ (t)
     (λ (θ)
        (+ (dot-product-2-1 (ref θ 0) t) (ref θ 1)))))
((linear t1) theta1)

(define relu
  (λ (t)
     (λ (θ)
        (rectify ((linear t) θ)))))

((relu t1) theta1)

(define 1-relu
  (λ (t)
     (λ (θ)
        ((relu t) θ))))
((1-relu t1) theta1)


(define 2-relu
  (λ (t)
     (λ (θ)
        ((relu
          ((relu t) θ))
         (refr θ 2)))))
(define theta2 (list w1 b1 w2 b2))
((2-relu t1) theta2)


(define 3-relu
  (λ (t)
     (λ (θ)
        ((2-relu
          ((relu t) θ))
         (refr θ 2)))))

(define theta3 (list w1 b1 w2 b2 w2 b2))
((3-relu t1) theta3)

(define k-relu
  (λ (k)
     (λ (t)
        (λ (θ)
           (cond
            [(zero? k) t]
            [else
             (((k-relu (sub1 k)) ((relu t) θ)) (refr θ 2))])))))

(((k-relu 1) t1) theta1)
(((k-relu 2) t1) theta2)
(((k-relu 3) t1) theta3)
