;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname bridge) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;               Yandong Zhu (20588720)
;;               CS 135, Winter 2016
;;               Assignment 4, Problem 3
;;****************************************************************
;;

;; (a)
;; A Bridgehand is one of
;; *empty
;; *a Str, whcih have the format "X Start" or "X End", where X is
;; one of "Spades", "Hearts", "Clubs", "Diamonds"
;; *a Sym, one of 'Ace, 'King, 'Queen, 'Jack
;; *a Nat in the range of 2 to 10 inclusive

;; my-bridgehand: list-of-Any -> Any
(define (my-bridgehand bridgehand)
  (cond [(empty? bridgehand) ...]
        [else (... (first bridgehand)...
                   (my-bridgehand (rest bridgehand))...)]))

;; Useful constants for examples and tests
(define list1 empty)
(define list2 (cons "Clubs Start" (cons 8 (cons "Clubs End" empty))))
(define list3 (cons "Spades Start"(cons 9(cons 'King
              (cons "Spades end"(cons "Hearts Start"
              (cons 'Queen(cons 'Ace(cons "Hearts End"
              (cons "Diamonds Start"(cons 8(cons 2(cons 'Jack
              (cons "Diamonds End" empty))))))))))))))
(define list4 (cons "Spades Start"(cons 9(cons 'King
              (cons "Spades end"(cons "Hearts Start"
              (cons 'Queen(cons 'Ace(cons "Hearts End"
              (cons "Clubs Start"(cons 8(cons 2(cons 'Jack
              (cons "Clubs End" (cons "Diamonds Start"
              (cons 7 (cons 'King (cons 'Ace
              (cons "Diamonds End" empty)))))))))))))))))))

;; (counts-point bridgehand) counts the value of a bridgehand
;; counts-point: listof Any -> Num
;; Examples:
(check-expect (count-points list2) 0)
(check-expect (count-points list3) 10)

(define (count-points bridgehand)
  (cond [(empty? bridgehand) 0]
        [else (cond [(number? (first bridgehand))
                     (count-points (rest bridgehand))]
                    [(string? (first bridgehand))
                     (count-points (rest bridgehand))]
                    [else (cond [(symbol=?
                                  'Ace (first bridgehand))
                                 (+ 4
                                    (count-points (rest bridgehand)))]
                                [(symbol=?
                                  'King (first bridgehand))
                                 (+ 3
                                    (count-points (rest bridgehand)))]
                                [(symbol=?
                                  'Queen (first bridgehand))
                                 (+ 2
                                    (count-points (rest bridgehand)))]
                                [(symbol=?
                                  'Jack (first bridgehand))
                                 (+ 1
                                    (count-points (rest bridgehand)))])])]))

;; Tests:
(check-expect (count-points list1) 0)
(check-expect (count-points list2) 0)
(check-expect (count-points list3) 10)
(check-expect (count-points list4) 17)