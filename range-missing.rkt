#lang racket

(provide range-missing)

(define (range-missing lon l-bound h-bound)
  (local
    [(define (member? x lst)
       (not (false? (member x lst))))]
    (cond
      [(= h-bound l-bound)
       (cond
         [(member? l-bound lon) empty]
         [else (cons l-bound empty)])]
      [(member? l-bound lon)
       (range-missing lon (add1 l-bound) h-bound)]
      [else  (cons l-bound
                   (range-missing lon (add1 l-bound) h-bound))])))

(range-missing '(1 2 4) 2 5)

