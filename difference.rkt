#lang racket

(provide difference)


(define (difference lst1 lst2)
  (local
    [(define (member? x lst) 
       (not (false? (member x lst))))
     (define (find-common lst1 lst2)
       (cond
         [(empty? lst1) empty]
         [(member? (first lst1) lst2)
          (cons (first lst1)
                (find-common (rest lst1) lst2))]
         [else (find-common (rest lst1) lst2)]))
     (define re-lst (find-common lst1 lst2))]
    (append (foldr (lambda (x y)
                          (cons x (filter (lambda (z)
                                            (not (equal? x z))) y)))
                   empty
                   (filter (lambda (x)
                             (not (member x re-lst))) lst1))
            (foldr (lambda (x y)
                          (cons x (filter (lambda (z)
                                            (not (equal? x z))) y)))
                   empty
                   (filter (lambda (x)
                             (not (member x re-lst))) lst2)))))

(difference '(1 2 3 4 1 2) '(1 2 3 5))