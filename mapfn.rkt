;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname mapfn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 08, Problem 1
;;****************************************************************
;;

;; a
;; (mapfn listof-sign arrgument): create a list of result
;; produced by the given list of operators and arrguments
;; mapfn: (listof (Num Num -> Any)) (listof Num Num) -> (listof Any)
;; Requiries
;; the listof sign should be the function that takes two num as
;; arrgument
;; the arrgument should be a list with two Num
;; Examples:
(check-expect (mapfn (list + -) (list 3 6)) (list 9 -3))
(check-expect (mapfn (list list make-posn) (list 3 56))
              (list (list 3 56) (make-posn 3 56)))

(define (mapfn listof-sign arrgument)
  (cond
    [(empty? listof-sign) empty]
    [else (cons
           ((first listof-sign)
            (first arrgument)
            (second arrgument))
           (mapfn (rest listof-sign) arrgument))]))

;; Tests
(check-expect (mapfn empty '(4 3)) empty)
(check-expect (mapfn (list + - * / list) '(3 2))
              '(5 1 6 1.5 (3 2)))

;;(is-in-order? listof-pair listof-op): first to check the given
;; operands is achieved the first condition of the first pair or not
;; if it achieves, move to the second function of the first pair.
;; else, move to the second pair. if the given bool cannot match any
;; of the condition, then produce 'error
;; is-in-order?: (listof (list (Any → Bool) (X X → Boolean))) (listof Any) → (anyof Bool ’error)
;; requires: first list is non-empty
;; Example:
(check-expect (is-in-order? (list (list symbol? symbol=?) (list integer? =)) (list 1 1 1)) true) 
(check-expect (is-in-order? (list (list integer? >)) (list 2 1 7)) false)

(define (is-in-order? listof-pair listof-op)
  (local
    [;; (check fcn-lst lst): to check the first operand has
     ;; a correspond predicate function in the function or not
     ;; check: (any -> bool) any -> bool
     (define (check fcn-lst op)
       (cond
         [(empty? fcn-lst) false]
         [else (or
                ((first (first fcn-lst)) op)
                (check (rest fcn-lst) op))]))
     ;; (list-order listof-fcn listof-op): predicate the order
     ;; in list of operands correspond to the second function  
     ;; of the pairs or not
     ;; list-order: (listof (list (Any → Bool) (X X → Boolean))) (listof Any) -> bool
     (define (list-order listof-fcn listof-op)
       (cond         
         [(empty? listof-fcn) false]
         [(and
           ((first (first listof-fcn)) (first listof-op))
           (in-order? (second (first listof-fcn)) listof-op)) true]
         [else (list-order (rest listof-fcn) listof-op)]))
     ;; (in-order? fcn listof-op): to check the list of 
     ;; operand accord to the given function or not
     ;; in-order: (any any -> bool) (listof any) -> bool
    (define (in-order? fcn listof-op)
      (cond 
        [(empty? (rest listof-op)) true]
        [else (and 
               (fcn (first listof-op) 
                    (second listof-op))
               (in-order? fcn (rest listof-op)))]))]
    (cond
      [(empty? listof-op) true]
      [(empty? (rest listof-op)) true]
      [(not (check listof-pair (first listof-op))) 'error]
      [else (list-order listof-pair listof-op)])))

;; Test:
(check-expect (is-in-order? (list (list integer? <)) empty) true)
(check-expect (is-in-order? (list (list integer? <)) (list 1)) true) 
(check-expect (is-in-order? (list (list integer? <)) (list 1 2 7)) true)
(check-expect (is-in-order? (list (list integer? >)) (list 1 2 7)) false)
(check-expect (is-in-order? (list (list string? string<?)) (list "1" "2" "7")) true)
(check-expect (is-in-order? (list (list integer? >) (list integer? <)) (list 1 1 1)) false)
(check-expect (is-in-order? (list (list integer? >) (list string? string>?)) (list 'a 'b 'c)) 'error)
(check-expect (is-in-order? (list (list integer? >) (list integer? =)) (list 1 1 1)) true)

