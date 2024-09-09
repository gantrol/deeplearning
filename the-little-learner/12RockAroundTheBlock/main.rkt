#lang racket
(require malt)

(list
    (list 64 32)
    (list 64)
    (list 45 64)
    (list 45)
    (list 26 45)
    (list 26))

(define block
    (lambda (fn shape-lst)
        (list fn shape-lst)))

(define block-fn
    (lambda (ba)
        (ref ba 0)))

(define block-ls
    (lambda (ba)
        (ref ba 1)))

(define layer1
    (block relu
        (list
            (list 64 32)
            (list 64))))

(define layer2
    (block relu
        (list
            (list 45 64)
            (list 45))))

(define layer3
    (block relu
        (list
            (list 26 45)
            (list 26))))


(define block-compose
    (lambda (f g j)
        (lambda (t)
            (lambda (theta)
                ((g
                    ((f t) theta))
                (tref theta j))))))

(define 2-relu
    (block-compose relu relu 2))
(block-compose relu relu 2)

(append (list 3 6 1) (list 7 2))

(define stack2
    (lambda (ba bb)
        (block
            (block-compose
                (block-fn ba)
                (block-fn bb)
                (len (block-ls ba)))
            (append
                (block-ls ba)
                (block-ls bb)))))


(define stacked-blocks
    (lambda (rbls ba)
        (cond
            [(null? rbls) ba]
            [else
                (stacked-blocks (refr rbls 1)
                    (stack2 ba (ref rbls 0)))])))

; List of blocks ->
(define stack-blocks
    (lambda (bls)
        (stacked-blocks (refr bls 1) (ref bls 0))))

(define 3-layer-network
    (stack-blocks
        (list
            layer1
            layer3
            layer3)))

(stack-blocks
    (list
        layer1
        layer2
        layer3))

(stacked-blocks
    (list
        layer2
        layer3)
    layer1)

(define dense-block
    (lambda (n m)
        (block relu
            (list
                (list m n)
                (list m)))))
