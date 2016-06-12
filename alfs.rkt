;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname alfs) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 09, Problem 1
;;****************************************************************
;;

;; a
;; (x-coords-of-posns lst) select a list of all the x-coordinates
;; from a list
;; x-coords-of-posns: (listof Any) -> (listof Any)
;; Example:
(check-expect (x-coords-of-posns (list 'a 1 3 (make-posn 4 5))) (list 4))
(check-expect (x-coords-of-posns (list 'a 1 3)) empty)

(define (x-coords-of-posns lst)
  (map posn-x (filter posn? lst)))

;; Tests:
(check-expect (x-coords-of-posns empty) empty)
(check-expect (x-coords-of-posns (list 'a 1 (make-posn 1 2) (make-posn 'a 'b))) (list 1 'a))

;; b
;; (alternating-sum): produces the alternating sum of a list 
;; a1 −a2 +a3 −a4 +···+(−1)^(n−1)an
;; alternating-sum : (listof Num) -> Num
;; Example:
(check-expect (alternating-sum '(1 2)) -1)
(check-expect (alternating-sum '(1 2 3)) 2)

(define (alternating-sum listof-num)
  (foldr - 0 listof-num))

;; Tests:
(check-expect (alternating-sum '()) 0)
(check-expect (alternating-sum '(2)) 2)

;; c
;; (remove-dulicates listof-num): remove the dulicates number in the list
;; and keep the first exist one
;; remove-dulicates: (listof Num) -> (listof Num)
;; Example:
(check-expect (remove-duplicates '(1 2 2 1 2)) '(1 2))
(check-expect (remove-duplicates '(1 2 3 4 5)) '(1 2 3 4 5))

(define (remove-duplicates listof-num)
  (foldr (lambda (x lst)
           (cons x (filter (lambda (y)
                             (not (= x y))) lst))) empty listof-num))

;; Tests:
(check-expect (remove-duplicates '()) '())
(check-expect (remove-duplicates '(1)) '(1))
(check-expect (remove-duplicates '(1 2 2 1 2 2 3)) '(1 2 3))

;; d
;; (first-col lst): select the first item of each list and cons them
;; to a new list
;; first-col: (listof (list of Num)) -> (listof Num)
;; Example:
(check-expect (first-col (list (list 1 2 3)
                               (list 2 3 4))) (list 1 2))
(check-expect (first-col (list (list 1 2 3))) (list 1))

(define (first-col lst)
  (map first lst))

;; Test:
(check-expect (first-col empty) empty)
(check-expect (first-col (list
                          (list 1 2 3 4)
                          (list 5 6 7 8)
                          (list 9 10 11 12)))
              (list 1 5 9))

;; e
;; (add1-mat lst): add1 on each element of the association list
;; add1-mat: (listof (listof Num)) -> (listof (listof Num))
(check-expect (add1-mat (list (list 1 2 3 4)
                              (list 1 2 3 4)))
              (list (list 2 3 4 5)
                    (list 2 3 4 5)))
(check-expect (add1-mat (list (list 2 3 5 8)))
              (list (list 3 4 6 9)))

(define (add1-mat lst)
  (map (lambda (x)
         (map add1 x)) lst))

;; Test:
(check-expect (add1-mat empty) empty)
(check-expect (add1-mat (list (list 5 7 1 6)
                              (list 1 2 3 4)
                              (list 8 3 2 0)))
              (list (list 6 8 2 7)
                    (list 2 3 4 5)
                    (list 9 4 3 1)))

;; f
;; (sum-at-zero listof-fcn): apply 0 to each function in the list
;; and sum them up
;; sum-at-zero: (listof function -> Num)
;; Requiries:
;; each function in the list should comsume and produce only one number
;; Example
(check-expect (sum-at-zero (list add1 add1 sub1)) 1)
(check-expect (sum-at-zero (list sub1 sub1 add1)) -1)

(define (sum-at-zero listof-fcn)
  (foldr + 0
         (foldr (lambda (x y)
                  (cons (x 0) y))
                empty listof-fcn)))

;; Test:
(check-expect (sum-at-zero empty) 0)
(check-expect (sum-at-zero (list add1)) 1)

