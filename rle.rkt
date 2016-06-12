;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname rle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 05, Problem 2
;;****************************************************************
;;

;; (a)
;; An list of RleList is one of
;; *empty
;; (cons (Any Nat) list-of-RleList)
;; requires:
;; Nat should be a non-zero nat

;; Useful Constants for examples or tests
(define list1 empty)
(define list2 (list (list 'red 3) (list 'blue 5)))
(define list3 (list (list 'yellow 7)))
(define list4 (list (list "black" 3) (list 'green 2) (list 3 6)))

;; (count num list), (rle-decode list): decode the run-length
;; encoded list into the original list
;; count: Num listof run-length-encoded -> listof Any
;; rle-decode: listof run-length-encoded -> listof Any
;; Examples:
(check-expect (rle-decode list2) (list 'red 'red 'red 'blue 'blue 'blue 'blue 'blue))

(define (rle-decode list)
  (cond [(empty? list) empty]
        [else (count (second (first list)) list)]))

(define (count num list)
  (cond [(> 1 num) (rle-decode (rest list))]
        [else (cons (first (first list))
                    (count (sub1 num) list))]))

;; Tests:
(check-expect (rle-decode list1) empty)
(check-expect (rle-decode list3) (list 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow 'yellow))
(check-expect (rle-decode list4) (list "black" "black" "black" 'green 'green 3 3 3 3 3 3))


