;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname card) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;  Yandong Zhu (20588720)
;;  CS 135 Winter 2016
;;  Assignment 03, Problem 4
;;****************************************************************
;;

(define-struct card (rank suit))
;; A card is a (make-card Nat Sym)
;; requires: 1 <= rank <= 13
;;           suit is one of ('spades 'hearts 'clubs 'diamonds)

;; my-card: Card -> Any
(define (my-card c)
  ( ... (card-rank) ...
    ... (card-suit) ...))

(define-struct hand (c1 c2 c3))
;; A hand is a (make-hand Card Card Card)
;; my-hand: Hand -> Any
(define (my-hand h)
  ( ... (hand-c1 h) ...
    ... (hand-c2 h) ...
    ... (hand-c3 h) ...))

;; Useful constants for examples and testing 
(define card1 (make-card 1 'spades))
(define card2 (make-card 3 'spades))
(define card3 (make-card 2 'clubs))
(define card4 (make-card 3 'hearts))
(define card5 (make-card 4 'hearts))
(define card6 (make-card 5 'hearts))
(define card7 (make-card 4 'diamonds))
(define card8 (make-card 4 'clubs))
(define card9 (make-card 7 'spades))
(define hand1 (make-hand card4 card5 card6))
(define hand2 (make-hand card1 card2 card9))
(define hand3 (make-hand card1 card2 card3))
(define hand4 (make-hand card5 card7 card8))
(define hand5 (make-hand card5 card7 card1))
(define hand6 (make-hand card5 card6 card9))

;; (better-hand card1 card2) Compare two card which has the higher
;; ranking
;; better-card: Card Card -> Card
;; Examples:
(check-expect (better-card card1 card2) card2)
(check-expect (better-card card2 card3) card2)

(define (better-card card1 card2)
  (cond [(symbol=? (card-suit card1) (card-suit card2))
         (make-card
          (max (card-rank card1) (card-rank card2))
          (card-suit card1))]
        [(symbol=? (card-suit card1) 'spades) card1]
        [(symbol=? (card-suit card2) 'spades) card2]
        [(symbol=? (card-suit card1) 'hearts) card1]
        [(symbol=? (card-suit card2) 'hearts) card2]
        [(symbol=? (card-suit card1) 'diamonds)card1]
        [(symbol=? (card-suit card2) 'diamonds) card2]))

;;Tests:
(check-expect (better-card card1 card2) card2)
(check-expect (better-card card2 card3) card2)
(check-expect (better-card card4 card9) card9)
(check-expect (better-card card5 card8) card5)
(check-expect (better-card card8 card6) card6)
(check-expect (better-card card7 card8) card7)
(check-expect (better-card card8 card7) card7)

;; (hand-value myhand) determine the highest hand type
;; hand-value: Hand -> Sym
;; Examples:
(check-expect (hand-value hand1) 'stright-flush)

(define (hand-value myhand)
  (cond [(and (symbol=? (card-suit (hand-c1 myhand))
                        (card-suit (hand-c2 myhand)))
              (symbol=? (card-suit (hand-c2 myhand))
                        (card-suit (hand-c3 myhand)))
              (= (- (max (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))
                    (min (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))) 2)
              (= (- (max (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))
                    (/ (+ (card-rank (hand-c1 myhand))
                          (card-rank (hand-c2 myhand))
                          (card-rank (hand-c3 myhand))) 3)) 1)) 'stright-flush]
        [(and (symbol=? (card-suit (hand-c1 myhand))
                        (card-suit (hand-c2 myhand)))
              (symbol=? (card-suit (hand-c2 myhand))
                        (card-suit (hand-c3 myhand)))) 'flush]
        [(and (= (- (max (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))
                    (min (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))) 2)
              (= (- (max (card-rank (hand-c1 myhand))
                         (card-rank (hand-c2 myhand))
                         (card-rank (hand-c3 myhand)))
                    (/ (+ (card-rank (hand-c1 myhand))
                          (card-rank (hand-c2 myhand))
                          (card-rank (hand-c3 myhand))) 3)) 1)) 'stright]
        [(= (card-rank (hand-c1 myhand))
            (card-rank (hand-c2 myhand))
            (card-rank (hand-c3 myhand))) 'three-of-a-kind]
        [(or (= (card-rank (hand-c1 myhand))
                (card-rank (hand-c2 myhand)))
             (= (card-rank (hand-c2 myhand))
                (card-rank (hand-c3 myhand)))
             (= (card-rank (hand-c3 myhand))
                (card-rank (hand-c1 myhand)))) 'pair]
        [else 'high-card]))
 
;; Tests:
(check-expect (hand-value hand1) 'stright-flush)
(check-expect (hand-value hand2) 'flush)
(check-expect (hand-value hand3) 'stright)
(check-expect (hand-value hand4) 'three-of-a-kind)
(check-expect (hand-value hand5) 'pair)
(check-expect (hand-value hand6) 'high-card)




