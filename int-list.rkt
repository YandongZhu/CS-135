;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname int-list) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;********************************************************
;;        Yandong Zhu  (20588720)
;;        CS 135, 2016 Winter
;;        Assignment 4, Problem 2
;;********************************************************
;;

;; Useful constants for examples and testing
(define base-number 0)
(define decimal-base 10)
(define list1 empty)
(define list2 (cons 4 (cons 8 (cons -5 (cons 7 empty)))))
(define list3 (cons 2 (cons 6 (cons 10 (cons 14 empty)))))
(define list4 (cons 2 (cons 6 (cons 7 empty))))
(define list5 (cons 2 empty))
(define list6 (cons 2 (cons 1 empty)))
(define list7 (cons 8 (cons 6 (cons 5 (cons 4 (cons 0 empty))))))


;; (a)
;; (elements-more-than list-of-int int) select the number from
;; given list and produce a new list whcih the number is
;; greater the given number
;; elements-more-than: listof Int -> listof Int
;; requires: A list-of-int is one of 
;; *empty
;; *(cons Int (list-of-int))
;; Example:
(check-expect (elements-more-than list2 8) empty)

(define (elements-more-than list-of-int int)
  (cond [(empty? list-of-int) empty]
        [(< int (first list-of-int)) (cons(first list-of-int)
                                          (elements-more-than
                                          (rest list-of-int) int))]
        [else (elements-more-than (rest list-of-int) int)]))

;; Tests:
(check-expect (elements-more-than list2 5) (cons 8 (cons 7 empty)))
(check-expect (elements-more-than list1 8) empty)

;; (b)
;; (judge list-of-int diff) check the consecutive numbers in
;; the list is equal to a given number or not
;; judge: listof Int -> Bool
;; requires: A list-of-int is one of 
;; *(cons Int (list-of-int))
;; Example:
(check-expect (judge list3 -4) true)
(check-expect (judge list3 2) false)

(define (judge list-of-int diff)
  (cond [(empty? (rest list-of-int)) true]
        [else (and (= diff
                      (- (first list-of-int)
                         (first (rest list-of-int))))
                   (judge (rest list-of-int) diff))]))

;; Tests:
(check-expect (judge list6 1) true)
(check-expect (judge list5 2) true)
(check-expect (judge list6 3) false)

;; (arithmetic-sequence list-of-int) check a list is an
;; arithmetic list or not
;; arithmetic-sequcnce: listof Int -> Bool
;; requires: A list-of-int is one of 
;; *empty
;; *(cons Int (list-of-int))
;; Examples:
(check-expect (arithmetic-sequence? list3) true)
(check-expect (arithmetic-sequence? list2) false)

(define (arithmetic-sequence? list-of-int)
  (cond [(empty? list-of-int) true]
        [(empty? (rest list-of-int)) true]
        [else (judge list-of-int (- (first list-of-int)
                                    (first(rest list-of-int))))]))
;; Tests:
(check-expect (arithmetic-sequence? list1) true)
(check-expect (arithmetic-sequence? list5) true)
(check-expect (arithmetic-sequence? list6) true)
(check-expect (arithmetic-sequence? list3) true)
(check-expect (arithmetic-sequence? list2) false)

;; (c)
;; (check list) check the number within the list
;; meet the requirements or not
;; check: listof Int -> Bool
;; requires: A list-of-int is one of 
;; *empty
;; *(cons Int (list-of-int))
;; Examples:
(check-expect (check list2) true)
(check-expect (check list4) false)

(define (check list-of-int)
  (cond [(empty? list-of-int) false]
        [else (or (> 0 (first list-of-int))
                  (< 9 (first list-of-int))
                  (check (rest list-of-int)))]))

(check-expect (check list2) true)
(check-expect (check list3) true)
(check-expect (check list4) false)

;; (sum list base-number) transfer the list into number
;; sum: listof Int -> Num
;; requires: A list-of-int is one of 
;; *empty
;; *(cons Int (list-of-int))
;; 0 <= Int < 10
;; Example
(check-expect (sum list4 base-number) 762)

(define (sum list-of-int base-number)
  (cond [(empty? list-of-int) 0]
        [else (+ (* (first list-of-int) (expt decimal-base base-number))
              (sum (rest list-of-int) (+ 1 base-number)))]))

;; Tests:
(check-expect (sum list4 base-number) 762)
(check-expect (sum list1 base-number) 0)
(check-expect (sum list5 base-number) 2)
(check-expect (sum list6 base-number) 12)

;; (digits->integer list) check a list achieve the requirements
;; or not, if it achieves, then produce the corresponding number
;; otherwise, produces eror
;; digits->integer: listof Int -> (Anyof Sym Num)
;; requires: A list-of-int is one of 
;; *empty
;; *(cons Int (list-of-int))
;; Examples:
(check-expect (digits->integer list2) 'error)
(check-expect (digits->integer list4) 762)

(define (digits->integer list-of-int)
  (cond [(empty? list-of-int) 0]
        [else (cond [(check list-of-int) 'error]
                    [else (sum list-of-int base-number)])]))
;; Test:
(check-expect (digits->integer list1) 0)
(check-expect (digits->integer list2) 'error)
(check-expect (digits->integer list3) 'error)
(check-expect (digits->integer list4) 762)
(check-expect (digits->integer list5) 2)
(check-expect (digits->integer list6) 12)
(check-expect (digits->integer list7) 4568)




