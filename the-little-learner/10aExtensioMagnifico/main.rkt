#lang racket
(require malt)

(define tmap vector-map) ; tmap in the book

(define sqrt-tensor-v1
  (lambda (t)
    (cond
      [(scalar? t) sqrt(t)]
      [else (tmap sqrt-tensor-v1 t)])))

(define of-rank?-v1
  (lambda (n t)
      (= (rank t) n)))

(define of-rank?
  (lambda (n t)
      (cond
        [(zero? n) (scalar? t)]
        [(scalar? t) #f]
        [else (of-rank? (sub1 n) (tref t 0))]
        )))

(of-rank? 3 (tensor (tensor (tensor 8) (tensor 9)) (tensor (tensor 4) (tensor 7))))


(define sqrt-tensor-v2
  (lambda (t)
    (cond
      [(of-rank? 0 t) sqrt(t)]
      [else (tmap sqrt-tensor-v2 t)])))

(define ext1-v1
  (lambda (f)
    (lambda (t)
      (cond
        [(of-rank? 0 t) (f t)]
        [else (tmap (ext1-v1 f) t)]))))

(define sqrt-tensor-v3
  (ext1-v1 sqrt-0))

(define zeroes-v1
  (ext1-v1 (lambda (x) 0)))

(define ext1-v2 ;final
  (lambda (f n)
    (lambda (t)
      (cond
        [(of-rank? n t) (f t)]
        [else (tmap (ext1-v2 f n) t)]))))

(define sqrt
  (ext1-v2 sqrt-0 0))

(define zeroes
  (ext1-v2 (lambda (t) 0.0) 0))

(define sum
  (ext1-v2 sum-1 1))

(define flatten
  (ext1-v2 flatten-2 2))

;; ext2
(define rank>-v1
  (lambda (t u)
    (> (rank t) (rank u))))

(define rank>
  (lambda (t u)
    (cond
      [(scalar? t) #f]
      [(scalar? u) #t]
      [else (rank> ((tref t 0) (tref u 0)))])))

(define of-ranks? ;takes arguments n, t, m, and u, 
  (lambda (n t m u) ;where n is the rank we're checking for tensor t, and m is the rank we're checking for tensor u.
    (cond
      [(of-rank? n t) (of-rank? m u)]
      [else #f]))
)

(of-ranks? 3 (tensor 3) 1 (tensor 1))
(of-ranks? 3 (tensor (tensor (tensor 5))) 1 (tensor 1))

(define desc-t
  (lambda (g t u)
    (tmap (lambda (et) (g et u)) t)))

(define desc-u
  (lambda (g t u)
    (tmap (lambda (eu) (g t eu)) u)))


(define desc
  (lambda (g n t m u)
    (cond
      ((of-rank? n t) (desc-u g t u))
      ((of-rank? m u) (desc-t g t u))
      ((= (tlen t) (tlen u)) (tmap g t u))
      ((rank> t u) (desc-t g t u))
      (else (desc-u g t u)))))

(define ext2
  (lambda (f n m)
    (lambda (t u)
      (cond
        [(of-ranks? n t m u) (f t u)]
        [else 
          (desc (ext2 f n m) n t m u)]))))

(define +
  (ext2 +-0-0 0 0))

(define *
  (ext2 *-0-0 0 0))

(define sqr
  (lambda (t)
    (* t t)))

; (define dot-product
;   (ext2 dot-product-1-1 1 1))
(define p (tensor (tensor 3 4 5) (tensor 7 8 9)))
(define t (tensor 2 4 3))
(define q (tensor (tensor 6 2) (tensor 4 9) (tensor 3 8)))
(define r (tensor (tensor 8 1) (tensor 7 3) (tensor 5 4)))
(* q r)











