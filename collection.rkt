;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname collection) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 06, Problem 3
;;****************************************************************
;;

(define-struct magazine (title issue))
;; A Magazine is a (make-magazine Str Nat)

;; Useful constant for examples and tests
(define magazine1 (make-magazine "apple" 1))
(define magazine2 (make-magazine "banana" 5))
(define magazine3 (make-magazine "cat" 3))
(define magazine4 (make-magazine "dog" 4))
(define magazine5 (make-magazine "dog" 6))
(define magazine6 (make-magazine "elephant" 3))
(define magazine7 (make-magazine "egg" 6))
(define magazine8 (make-magazine "egg" 4))
(define magazine9 (make-magazine "apple" 3))
(define list1 (list magazine5 magazine4 magazine3 magazine1 magazine2))
(define list2 (list magazine1 magazine7 magazine8 magazine4 magazine6))
(define list3 (list magazine4 magazine5))
(define list4 (list magazine4 magazine5 magazine6))
(define list5 (list (list "apple" (list 1)) (list "banana" (list 5))
                    (list "cat" (list 3)) (list "dog" (list 4 6))))
(define list6 (list (list "dog" (list 4 6)) (list "elephant" (list 3))))
(define list7 (list magazine1 magazine2 magazine3 magazine4 magazine5))
(define list8 (list magazine1 magazine2 magazine2 magazine3 magazine4 magazine4 magazine5))
(define list9 (list magazine1 magazine2 magazine3 magazine3 magazine4 magazine4 magazine5 magazine5))

;; (a)
;; (magazine<? magazine magazine2): compare the two given magazine that
;; obey the lexicographical rule or not
;; magazine<?: magazine magazine -> bool
;; example:
(check-expect (magazine<? magazine1 magazine2) true)

(define (magazine<? magazine1 magazine2)
  (cond [(string<? (magazine-title magazine1)
                   (magazine-title magazine2)) true]
        [(string=? (magazine-title magazine1)
                   (magazine-title magazine2))
         (cond [(< (magazine-issue magazine1)
                   (magazine-issue magazine2)) true]
               [else false])]
        [else false]))

;; Tests
(check-expect (magazine<? magazine1 magazine2) true)
(check-expect (magazine<? magazine4 magazine5) true)
(check-expect (magazine<? magazine7 magazine8) false)
(check-expect (magazine<? magazine6 magazine3) false)


;; (b)
;; (sort-magazines list-of-magazine): sort a list of
;; magazine and create a new list of magazine based on
;; the lexicographical rule
;; sort-magazines: listof magazine -> listof magazine
;; Examples:
(check-expect (sort-magazines list1)
              (list magazine1 magazine2 magazine3 magazine4 magazine5))

(define (sort-magazines list-of-magazine)
  (cond [(empty? list-of-magazine) empty]
        [else (insert (first list-of-magazine)
                      (sort-magazines (rest list-of-magazine)))]))

;; Tests:
(check-expect (sort-magazines list1)
              (list magazine1 magazine2 magazine3 magazine4 magazine5))
(check-expect (sort-magazines list2)
              (list magazine1 magazine4 magazine8 magazine7 magazine6))


;; (insert n list): insert a magazine into a list of magazine by the
;; lexicographical rule
;; insert: magazine, listof magazine -> listof magazine

(define (insert n list)
  (cond [(empty? list) (cons n list)]
        [(magazine<? n (first list))
         (cons n list)]
        [else (cons (first list)
                    (insert n (rest list)))]))


;; (c)
;; (need-between listof-magazine title low-nat high-nat): prouduce a
;; list of issue that is not between the issue boundary
;; need-between: listof magazine, Str, Num, Num -> listof Num
;; Requeries:
;; low-nat <= high-nat
;; Example
(check-expect (need-between list7 "egg" 3 7) (list 3 4 5 6 7))

(define (need-between listof-magazine title low-nat high-nat)
  (cond [(have? listof-magazine title)
         (count1 low-nat high-nat)]
        [else (count2 (de-repeat (select-list listof-magazine title)
                                 (first listof-magazine) true) low-nat high-nat)]))

;; Tests
(check-expect (need-between list7 "dog" 2 5) (list 2 3 5))
(check-expect (need-between list7 "dog" 4 7) (list 5 7))
(check-expect (need-between list7 "dog" 2 7) (list 2 3 5 7))
(check-expect (need-between list8 "dog" 2 5) (list 2 3 5))
(check-expect (need-between list9 "dog" 2 5) (list 2 3 5))
(check-expect (need-between list7 "dg" 2 5) (list 2 3 4 5))
(check-expect (need-between list7 "dog" 5 6) (list 5))

;; (de-repeat list magazine switch): eliminate the duplicate magazines
;; in the list
;; de-repeat: listof magazine magazine bool -> listof magazine

(define (de-repeat list magazine switch)
  (cond [(empty? list) empty]
        [(and switch (magazine=? magazine (first list)))
         (cons (first list)
               (de-repeat (rest list) magazine false))]
        [(magazine=? magazine (first list))
         (de-repeat (rest list) magazine false)]
        [else (de-repeat list (first list) true)]))

;; (have? list title):check the title is exist in the list or not
;; have?: listof magazine, Str -> bool

(define (have? list title)
  (cond [(empty? list) true]
        [(string=? title
                   (magazine-title (first list))) false]
        [else (have? (rest list) title)]))

;; (count1 low high): create a list of number between
;; the low and high number (including)
;; count1: Num, Num -> listof Num

(define (count1 low high)
  (cond [(= high low) (cons high empty)]
        [else (cons low (count1 (add1 low) high))]))

;; (select-list list title): select the list of magazine
;; by the given title
;; select-list: listof magazine, Str -> listof magazine

(define (select-list list title)
  (cond [(empty? list) empty]
        [(string=? title (magazine-title (first list)))
         (cons (first list) (select-list (rest list) title))]
        [else (select-list (rest list) title)]))

;; (count2 list low high): create a list of number that
;; between the low and high number and not exist in the
;; list
;; count2: listof magazine, Num, Num -> listof Num
(define (count2 list low high)
  (cond [(empty? list) (count1 low high)]
        [(= low high)
         (cond [(= low (magazine-issue (first list))) empty]
               [else (cons high empty)])]
        [(< low (magazine-issue (first list)))
         (cons low (count2 list (add1 low) high))]
        [(> low (magazine-issue (first list)))
         (count2 (rest list) low high)]
        [(= low (magazine-issue (first list)))
         (count2 (rest list) (add1 low) high)]))

;; (d)
;; (magazine=? magazine1 magazine2): to check two magazine is
;; equal or not
;; magazine=?: magazine, magazine -> bool

(define (magazine=? magazine1 magazine2)
  (cond [(and (string=? (magazine-title magazine1)
                        (magazine-title magazine2))
              (= (magazine-issue magazine1)
                 (magazine-issue magazine2))) true]
        [else false]))

;; (same-list magazine listof-magazine): to check two given 
;; list is equal or not
;; is-member?: listof magazine, listof magazine -> bool

(define (same-list? list1 list2)
  (cond [(empty? list2) true]
        [(empty? list1) false]
        [(magazine=? (first list1) (first list2))
         (same-list? (rest list1) (rest list2))]
        [else false]))

;; (magazine-lists-equal-? list1 list2) compare the owner list with
;; the completed list, to check the owner has completed his book or not
;; magazine-list-equal?: listof-magazine listof-magazine -> bool
;; requiries:
;; list1 is the owner list, list2 is the completed list
;; list1 should be equal or shorter than list2
;; Examples:
(check-expect (magazine-lists-equal? list1 list1) true)

(define (magazine-lists-equal? list1 list2)
 (same-list? (sort-magazines list1) (sort-magazines list2)))

;; Tests:
(check-expect (magazine-lists-equal? list1 list1) true)
(check-expect (magazine-lists-equal? list1 list2) false)
(check-expect (magazine-lists-equal? list3 list4) false)

;; (e)
;; (is-member? magazine list1): to check a magzine is in the
;; list of magazine or not
;; is-member?: magazine, listof magazine -> bool
(define (is-member? magazine list1)
  (cond [(empty? list1) false]
        [(magazine=? magazine (first list1)) true]
        [else (is-member? magazine (rest list1))]))

;; (combine list1 list2): combine two list together
;; combine: listof magazine, listof magazine -> listof magazine

(define (combine list1 list2)
  (cond [(empty? list1) list2]
        [(is-member? (first list1) list2)
         (combine (rest list1) list2)]
        [else (combine (rest list1)
               (cons (first list1) list2))]))

;; (merge-collections list1 list2): combine two list of magazine
;; together
;; merge-collections: listof magazine, listof magazine -> listof magazine
;; Example:
(check-expect (merge-collections list3 list4) list4)

(define (merge-collections list1 list2)
  (sort-magazines (combine list1 list2)))

;; Tests:
(check-expect (merge-collections list1 list2) (list magazine1 magazine2 magazine3 magazine4 magazine5 magazine8 magazine7 magazine6))
(check-expect (merge-collections list2 list3) (list magazine1 magazine4 magazine5 magazine8 magazine7 magazine6))
(check-expect (merge-collections list1 list3) (list magazine1 magazine2 magazine3 magazine4 magazine5))

;; (f)
;; (a list magazine): create a list of issue by the given 
;; magazine title
;; a: listof magazine, magazine -> list of Num

(define (a list magazine)
  (cond [(empty? list) empty]
        [(string=? (magazine-title magazine)
                   (magazine-title (first list)))
               (cons (magazine-issue (first list))
                     (a (rest list) magazine))]
        [else empty]))

;; (b list): create a associated list with the first element is the
;; title and the second element is the list of issue
;; b: listof magazine -> listof (Str listof Num)
(define (b list)
  (cons (magazine-title (first list))
         (cons (a list (first list)) empty)))

;; (sort-index list magazine switch): create an associated list that list of
;; the list the (b list) created
;; sort-index: listof magazine, magazine, bool -> listof listof (Str, listof Num) 

(define (sort-index list magazine switch)
  (cond [(empty? list) empty]
        [(and switch
              (string=? (magazine-title magazine)
                        (magazine-title (first list))))
         (cons (b list)
               (sort-index (rest list) magazine false))]
        [(string=? (magazine-title magazine)
                   (magazine-title (first list)))
         (sort-index (rest list) magazine false)]
        [else (sort-index list (first list) true)]))

;; (create-index listof-magazine): create an index of a magazine
;; An Index is one of:
;; * empty
;; * (cons (list Str (listof Nat)) Index)
;; where (listof Nat) is a non-empty list
;; create-index: listof magazine -> listof listof (Str, listof Num)
;; Example:
(check-expect (create-index list7) list5)

(define (create-index listof-magazine)
  (sort-index (de-repeat listof-magazine
                        (first listof-magazine) true)
             (first (de-repeat listof-magazine
                               (first listof-magazine) true)) true))

;; Tests:
(check-expect (create-index list4) list6)
(check-expect (create-index list8) list5)
(check-expect (create-index list9) list5)

;; (g)
;; (own-magazine? index magazine): to check a magazine
;; is in the given index or not
;; own-magazine?: associtaed list, magazine -> bool
;; example:
(check-expect (own-magazine? list5 magazine1) true)

(define (own-magazine? index magazine)
  (cond [(empty? index) false]
        [(string=? (magazine-title magazine)
                   (first (first index)))
         (issue? (second (first index)) magazine)]
        [else (own-magazine? (rest index) magazine)]))

;; Tests:
(check-expect (own-magazine? list5 magazine2) true)
(check-expect (own-magazine? list5 magazine8) false)
(check-expect (own-magazine? list5 magazine9) false)

;; (issue? list magazine): check the issue is in the list
;; or not
;; issue: listof Num, magazine -> bool
(define (issue? list magazine)
  (cond [(empty? list) false]
        [(= (first list)
            (magazine-issue magazine)) true]
        [else (issue? (rest list) magazine)]))





;;(h)

(define (need-magazines index title issue)
  (cond [(exist? index title)
         (string-append title ": need all")]
        [(equal (second (first index)) issue 0)
         (string-append title ": completed")]
        [else (find (second (select index title))
                    issue 0 (string-append title ": "))]))

;; find the name is in the index or not
(define (exist? index name)
  (cond [(empty? index) true]
        [(string=? (first (first index))
                   name) false]
        [else (exist? (rest index) name)]))

;; select the list from index that match the name
(define (select index name)
  (cond [(string=? (first (first index))
                   name) (first index)]
        [else (select (rest index) name)]))

(define (equal list issue start)
  (cond [(= issue start) true]
        [(empty? list) false]
        [(= (first list) (add1 start))
         (equal (rest list) issue (add1 start))]
        [else false]))

(define (find list issue start title)
  (cond [(= start issue)
         (substring title 0 (- (string-length title) 2))]
        [(empty? list) (find list issue (add1 start)
                             (string-append title
                                            (number->string (add1 start))
                                            ", "))]
        [(= (first list) (add1 start))
         (find (rest list) issue (add1 start) title)]
        [else (find list issue (add1 start)
                    (string-append title
                                   (number->string (add1 start))
                                   ", "))]))

(check-expect (need-magazines list5 "apple" 1) "apple: completed")
(check-expect (need-magazines list5 "frog" 8) "frog: need all")
