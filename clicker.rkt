;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname clicker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 06, Problem 1
;;****************************************************************
;;

;; Useful constants for the problem
(define count-percent 0.75)
(define correct-answer 2)
(define wrong-answer 1)
(define total-mark 100)

;; Usefull constants for test or examples
(define answer-list1 (list 'a 'b 'c 'none ))
(define correct-list1 (list 'a 'b 'c 'd))
(define answer-list2 (list 'b 'a 'none 'b))
(define answer-list3 (list 'none 'none 'none 'none))
(define answer-list4 (list 'none  'b 'none 'c))
(define answer-list5 (list 'none 'c 'none 'none))


;; (clicker-grade answer-list correct-list):calculate the total
;; grade of clicker by given answer list and correct list
;; clicker-grade: listof Sym, listof Sym -> Num
;; requires:
;; answer-list will be any of: 'a, 'b, 'c, 'd, 'e, 'none
;; correct-list will be any of: 'a, 'b, 'c, 'd, 'e
;; example:
(check-expect (clicker-grade answer-list1 correct-list1) 100)

(define (clicker-grade answer-list correct-list)
 (clicker-mark (total correct-list)
               (count-correct answer-list correct-list)
               (- (total correct-list)
                  (count-correct answer-list correct-list)
                  (unanswer answer-list)))) 
;; Tests:
(check-expect (clicker-grade answer-list2 correct-list1) 50)
(check-expect (clicker-grade answer-list3 correct-list1) 0)
(check-expect (clicker-grade answer-list4 correct-list1) 50)
(check-expect (clicker-grade answer-list5 correct-list1) 100/6)

;; (total correct-list): count the total question numbers
;; total: listof Sym -> Num
(define (total correct-list)
  (cond [(empty? correct-list) 0]
        [else (add1 (total (rest correct-list)))]))


;; (count-correct answer-list correct-list): count the total
;; correct answer number
;; count-correct: listof Sym, listof Sym -> Num
(define (count-correct answer-list correct-list)
  (cond [(empty? answer-list) 0]
        [(symbol=? (first answer-list)
                   (first correct-list))
         (add1 (count-correct (rest answer-list)
                              (rest correct-list)))]
        [else (count-correct (rest answer-list)
                             (rest correct-list))]))

;; (unanswer answer-list): count the total unanswer number
;; unanswer: listof Sym -> Num
(define (unanswer answer-list)
  (cond [(empty? answer-list) 0]
        [(symbol=? 'none (first answer-list))
         (add1 (unanswer (rest answer-list)))]
        [else (unanswer (rest answer-list))]))


;; (clicker-mark total correct incorrect): calculate the
;; clicker mark by given total number of question, correct
;; number of question and incorrect number of question
;; clicker-mark: Num Num Num -> Num
(define (clicker-mark total correct incorrect)
  (* (/ (+ (* correct-answer (min correct
                     (* count-percent total)))
           (* wrong-answer (min incorrect
                     (max 0 (- (* count-percent total)
                               correct)))))
        (* total count-percent correct-answer)) total-mark))


