;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 09, Problem 4
;;****************************************************************
;;

;; useful constant for example and test 
(define c0 (lambda (f) (lambda (x) x)))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))
(define c3 (lambda (f) (lambda (x) (f (f (f x))))))
(define c4 (lambda (f) (lambda (x) (f (f (f (f x)))))))
(define cj (lambda (f) (lambda (x) (f ... (f x) ... ))))

;; a
;; (c->nat cj): give a cj and produce j, where j is a Num
;; the definition of cj is defined priviously
;; c->nat: ((X -> X) X -> X) -> Nat
;; Example:
(check-expect (c->nat c2) 2)
(check-expect (c->nat c3) 3)

(define (c->nat cj)
  ((cj add1) 0))

;; Tests:
(check-expect (c->nat c0) 0)
(check-expect (c->nat c4) 4)

;; b
;; (nat->c n): given a nature number n and produce the
;; corresponding function cn
;; nat->c: Nat -> ((X -> X) X -> X)
;; Example:
(check-expect (((nat->c 1) add1) 2) 3)
(check-expect (((nat->c 2) sub1) 2) 0)

(define (nat->c n)
  (lambda (f)
    (lambda (x)
      (local
        [;; (d num): create the result by the given num
         ;; d: Nat -> ((X -> X) X -> X)
         (define (d num)
           (cond
             [(= num 0) x]
             [else (f (d (sub1 num)))]))
         ;; the result of the function
         ;; c: ((X -> X) X -> X)  
         (define c
           (d n))]
        c))))

;; Tests
(check-expect (((nat->c 0) add1) 2) 2)
(check-expect (((nat->c 3) (lambda (x)
                             (/ 1 x))) 2) 0.5)




