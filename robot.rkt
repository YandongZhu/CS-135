;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname robot) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;  Yandong Zhu (20588720)
;;  CS 135 Winter 2016
;;  Assignment 03, Problem 2
;;****************************************************************
;;

;; Useful constants for examples and testing
(define a (make-posn 2 2))
(define b (make-posn 3 5))

;; (robot place ini-dire turn-dire distance) Produces the new
;; position after robot taking action
;; robot: Posn Sym Sym Num -> Posn
;; requires: ini-dire is one of ('north 'south 'west 'east)
;;           turn-dire is one of ('right 'left 'noturn)
;;           0 <= distance
;; Examples:
(check-expect (robot a 'north 'right 5) (make-posn 7 2))
(check-expect (robot b 'west 'noturn 2) (make-posn 1 5))

(define (robot place ini-dire turn-dire distance)
  (cond [(symbol=? ini-dire 'north)
         (cond [(symbol=? turn-dire 'left)
                (make-posn (- (posn-x place) distance) (posn-y place))]
               [(symbol=? turn-dire 'right)
                (make-posn (+ (posn-x place) distance) (posn-y place))]
               [(symbol=? turn-dire 'noturn)
                (make-posn (posn-x place) (+ (posn-y place) distance))])]
        [(symbol=? ini-dire 'west)
         (cond [(symbol=? turn-dire 'left)
                (make-posn (posn-x place) (- (posn-y place) distance))]
               [(symbol=? turn-dire 'right)
                (make-posn (posn-x place) (+ (posn-y place) distance))]
               [(symbol=? turn-dire 'noturn)
                (make-posn (- (posn-x place) distance) (posn-y place))])]
        [(symbol=? ini-dire 'south)
         (cond [(symbol=? turn-dire 'left)
                (make-posn (+ (posn-x place) distance) (posn-y place))]
               [(symbol=? turn-dire 'right)
                (make-posn (- (posn-x place) distance) (posn-y place))]
               [(symbol=? turn-dire 'noturn)
                (make-posn (posn-x place) (- (posn-y place) distance))])]
        [(symbol=? ini-dire 'east)
         (cond [(symbol=? turn-dire 'left)
                (make-posn (posn-x place) (+ (posn-y place) distance))]
               [(symbol=? turn-dire 'right)
                (make-posn (posn-x place) (- (posn-y place) distance))]
               [(symbol=? turn-dire 'noturn)
                (make-posn (+ (posn-x place) distance) (posn-y place))])]))

;; Tests
(check-expect (robot a 'north 'right 5) (make-posn 7 2))
(check-expect (robot a 'north 'left 5) (make-posn -3 2))
(check-expect (robot a 'north 'noturn 5) (make-posn 2 7))
(check-expect (robot b 'west 'right 5) (make-posn 3 10))
(check-expect (robot b 'west 'left 5) (make-posn 3 0))
(check-expect (robot b 'west 'noturn 5) (make-posn -2 5))
(check-expect (robot a 'south 'right 5) (make-posn -3 2))
(check-expect (robot a 'south 'left 5) (make-posn 7 2))
(check-expect (robot a 'south 'noturn 5) (make-posn 2 -3))
(check-expect (robot b 'east 'right 5) (make-posn 3 0))
(check-expect (robot b 'east 'left 5) (make-posn 3 10))
(check-expect (robot b 'east 'noturn 5) (make-posn 8 5))




 
