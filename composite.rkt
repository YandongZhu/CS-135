;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname composite) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 09, Problem 2
;;****************************************************************
;;

;; a
;; (compostie a b): produce the composite function of given 
;; two function
;; compostie: function function -> function
;; Requiries:
;; the consume two function should consume one parameter each
;; Example:
(check-expect ((composite abs add1) -4) 3)
(check-expect ((composite add1 abs) -4) 5)

(define (composite a b)
  (lambda (x)
    (a (b x))))

;; Tests:
(check-expect ((composite add1 sub1) 3) 3)

;; b
;; (inverse-of-square-list listof-num): inverse each element in
;; the list first then square them
;; inverse-of-square-list: listof Num -> listof Num
;; Example:
(check-expect (inverse-of-square-list (list 1 2)) (list 1 0.25))
(check-expect (inverse-of-square-list (list 8)) (list 0.015625))

(define (inverse-of-square-list listof-num)
  (map (composite sqr (lambda (x)
                        (/ 1 x))) listof-num))

;; Test:
(check-expect (inverse-of-square-list empty) empty)

;; c
;; (composite-list listof-fcn): composite the list of function
;; composite-list: listof function -> function
;; Requiries:
;; the list of function should consume one parameter each
;; Example:
(check-expect ((composite-list (list add1 sub1)) 4) 4)
(check-expect ((composite-list (list add1 abs sub1 add1)) 4) 5)

(define (composite-list listof-fcn)
  (foldr composite (lambda (x)
                     x) listof-fcn))

;; Tests:
(check-expect ((composite-list empty) 43) 43)
(check-expect ((composite-list (list add1 sub1 (lambda(x)
                                                 (/ 1 x)))) 0.5) 2)
