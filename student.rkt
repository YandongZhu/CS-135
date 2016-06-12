;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname student) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 06, Problem 2
;;****************************************************************
;;

;; Useful constant for examples and tests
(define name-list1 (list 'a 'b 'c 'd))
(define name-list2 (list 'a))
(define height-list1 (list 183 138 189 189))
(define height-list2 (list 183 138 138 189))
(define height-list3 (list 184))
(define standard1 189)
(define standard2 138)
(define standard3 168)
(define AL1 (list (list 'a 183)
                  (list 'b 138)
                  (list 'c 189)
                  (list 'd 189)))
(define AL2 (list (list 'a 183)
                  (list 'b 138)
                  (list 'c 138)
                  (list 'd 189)))
(define AL3 (list (list 'a 184)))

;; (a)
;; (tallest name-list height-list): select the highest student
;; from the list and return his name
;; tallest: listof Sym, listof Num -> Sym
;; examples:
(check-expect (tallest name-list1 height-list1) 'c)

(define (tallest name-list height-list)
  (cond [(empty? (rest name-list)) (first name-list)]
        [(>= (first height-list)
             (second height-list))
         (tallest (append (list (first name-list))
                          (rest (rest name-list)))
                  (append (list (first height-list))
                          (rest (rest height-list))))]
        [else (tallest (rest name-list)
                       (rest height-list))]))

(check-expect (tallest name-list2 height-list3) 'a)
(check-expect (tallest name-list1 height-list2) 'd)


;; (b)
;; (shortest name-list height-list): select the shortest student
;; from the list and return his name
;; shortest: listof Sym, listof Num -> Sym
;; Example:
(check-expect (shortest name-list1 height-list1) 'b)

(define (shortest name-list height-list)
  (select (rest name-list)
          (rest height-list)
          (first name-list)
          (first height-list)))

(check-expect (shortest name-list2 height-list3) 'a)
(check-expect (shortest name-list1 height-list2) 'b)

(define (select name-list height-list name height)
  (cond [(empty? name-list) name]
        [else (cond [(<= height (first height-list))
                     (select (rest name-list)
                             (rest height-list)
                             name height)]
                    [else (select (rest name-list)
                                  (rest height-list)
                                  (first name-list)
                                  (first height-list))])]))


;; (c)
;; (student-al name-list height-list): create an associated list that
;; list of name and his corrsponding height
;; student-al: listof Sym, listof Num -> listof listof (Sym Num)
;; Examples:
(check-expect (student-al name-list1 height-list1) AL1)

(define (student-al name-list height-list)
  (cond [(empty? name-list) empty]
        [else (cons (list (first name-list)
                          (first height-list))
                    (student-al (rest name-list)
                                (rest height-list)))]))

;; Tests:
(check-expect (student-al name-list1 height-list2) AL2)
(check-expect (student-al name-list2 height-list3) AL3)              

;; (d)
;; (basketball list standard): select the students that achieve the
;; standard and create a new list of their name
;; basketball: associated-list Num -> listof Sym
;; Examples:
(check-expect (basketball AL1 standard1) (list 'c 'd))
(check-expect (basketball AL1 standard3) (list 'a 'c 'd))

(define (basketball list standard)
  (cond [(empty? list) empty]
        [(>= (second (first list)) standard)
          (cons (first (first list))
                (basketball (rest list) standard))]
        [else (basketball (rest list) standard)]))

;; Tests:
(check-expect (basketball AL1 standard2) (list 'a 'b 'c 'd))
(check-expect (basketball AL2 standard3) (list 'a 'd))
(check-expect (basketball AL3 standard1) empty)
(check-expect (basketball AL3 standard2) (list 'a))