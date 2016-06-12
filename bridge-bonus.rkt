;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bridge-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;               Yandong Zhu (20588720)
;;               CS 135, Winter 2016
;;               Assignment 4, Problem 3
;;****************************************************************
;;

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


(define (count1 string1 string2 bridgehand)
  (cond [(empty? bridgehand) 0]
        [(and (string? (first bridgehand))
              (string=? string1 (first bridgehand)))
         (count2 string1 string2 (rest bridgehand))]
        [else (count1 string1 string2 (rest bridgehand))]))


(define (count2 string1 string2 bridgehand)
  (cond
    [(empty? bridgehand) 0]
    [(and (string? (first bridgehand))
          (string=? string2 (first bridgehand)))
     (count1 string1 string2 (rest bridgehand))]
    [else (+ 1 (count2 string1 string2 (rest bridgehand)))]))


(define (suit bridgehand)
  (cons (count1 "Spades Start" "Spades End" bridgehand)
        (cons (count1 "Hearts Start" "Hearts End" bridgehand)
              (cons (count1 "Clubs Start" "Clubs End" bridgehand)
                    (cons (count1 "Diamonds Start" "Diamonds End" bridgehand)
                          empty)))))


(define (sum bridgehand)
  (cond  [(empty? bridgehand) 0]
         [(= 0 (first bridgehand)) (+ 3 (sum (rest bridgehand)))]
         [(= 1 (first bridgehand)) (+ 2 (sum (rest bridgehand)))]
         [(= 2 (first bridgehand)) (+ 1 (sum (rest bridgehand)))]
         [else (sum (rest bridgehand))]))

(define (count-distribution bridgehand)
  (sum (suit bridgehand)))

(check-expect (count-distribution list3) 4)


