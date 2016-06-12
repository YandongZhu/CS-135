;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intersect) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (intersect lst1 lst2)
  (local
    [(define (find-common lst1 lst2 lst)
       (cond
         [(empty? lst1) lst]
         [(member? (first lst1) lst2)
          (find-common (rest lst1) lst2 (append lst (list (first lst1))))]
         [else (find-common (rest lst1) lst2 lst)]))]
    (find-common (foldr (lambda (x y)
                          (cons x (filter (lambda (z)
                                            (not (equal? x z))) y))) empty lst1) lst2 empty)))

(intersect '(1 2 3) '(2 3))