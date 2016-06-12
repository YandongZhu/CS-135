;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname notes) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 03, Problem 3
;;****************************************************************
;;

;;(a)
(define-struct note (letter modifier))
;; A 3dposn is a (make-note Sym Sym)
;; Requires:
;; leter is one of the ('A, 'B, 'C, 'D, 'E, 'F, 'G)
;; modifier is one of the  ('nature, 'flat, 'sharp)
;; my-note: note -> Any
(define (my-note n)
  ( ... (note-letter n) ...
    ... (note-modifier n) ...))

;; Useful constant for example and tests
(define note1 (make-note 'A 'sharp))
(define note2 (make-note 'A 'flat))
(define note3 (make-note 'A 'nature))
(define note4 (make-note 'B 'sharp))
(define note5 (make-note 'B 'flat))
(define note6 (make-note 'B 'nature))
(define note7 (make-note 'C 'sharp))
(define note8 (make-note 'C 'flat))
(define note9 (make-note 'C 'nature))
(define note10 (make-note 'D 'sharp))
(define note11 (make-note 'D 'flat))
(define note12 (make-note 'D 'nature))
(define note13 (make-note 'E 'sharp))
(define note14 (make-note 'E 'flat))
(define note15 (make-note 'E 'nature))
(define note16 (make-note 'F 'sharp))
(define note17 (make-note 'F 'flat))
(define note18 (make-note 'F 'nature))
(define note19 (make-note 'G 'sharp))
(define note20 (make-note 'G 'flat))
(define note21 (make-note 'G 'nature))
(define list1 empty)
(define list2 (list note2 note6 note9 note15 note20))
(define list3 (list note4 note7 note8))

;;(b)
;; (normalize-note note1): to tranfer the note into
;; correspond number
;; normalie-note: Note -> Num
;; requires:
;; (make-note 'C 'flat) should be better wriitten in (make-note 'B 'nature)
;; (make-note 'F 'flat) should be better wriitten in (make-note 'E 'nature)
;; (make-note 'B 'sharp) should be better wriitten in (make-note 'C 'nature)
;; (make-note 'E 'sharp) should be better wriitten in (make-note 'F 'nature)
;; Examples:
(check-expect (normalize-note note1) 11)
(check-expect (normalize-note note7) 2)

(define (normalize-note note1)
  (cond [(symbol=? (note-letter note1) 'C)
         (cond [(symbol=? (note-modifier note1) 'nature) 1]
               [(symbol=? (note-modifier note1) 'flat) 12]
               [(symbol=? (note-modifier note1) 'sharp) 2])]
        [(symbol=? (note-letter note1) 'D)
         (cond [(symbol=? (note-modifier note1) 'nature) 3]
               [(symbol=? (note-modifier note1) 'flat) 2]
               [(symbol=? (note-modifier note1) 'sharp) 4])]
        [(symbol=? (note-letter note1) 'E)
         (cond [(symbol=? (note-modifier note1) 'nature) 5]
               [(symbol=? (note-modifier note1) 'flat) 4]
               [(symbol=? (note-modifier note1) 'sharp) 6])]
        [(symbol=? (note-letter note1) 'F)
         (cond [(symbol=? (note-modifier note1) 'nature) 6]
               [(symbol=? (note-modifier note1) 'flat) 5]
               [(symbol=? (note-modifier note1) 'sharp) 7])]
        [(symbol=? (note-letter note1) 'G)
         (cond [(symbol=? (note-modifier note1) 'nature) 8]
               [(symbol=? (note-modifier note1) 'flat) 7]
               [(symbol=? (note-modifier note1) 'sharp) 9])]
        [(symbol=? (note-letter note1) 'A)
         (cond [(symbol=? (note-modifier note1) 'nature) 10]
               [(symbol=? (note-modifier note1) 'flat) 9]
               [(symbol=? (note-modifier note1) 'sharp) 11])]
        [(symbol=? (note-letter note1) 'B)
         (cond [(symbol=? (note-modifier note1) 'nature) 12]
               [(symbol=? (note-modifier note1) 'flat) 11]
               [(symbol=? (note-modifier note1) 'sharp) 1])]))

;; Tests:
(check-expect (normalize-note note1) 11)
(check-expect (normalize-note note2) 9)
(check-expect (normalize-note note3) 10)
(check-expect (normalize-note note4) 1)
(check-expect (normalize-note note5) 11)
(check-expect (normalize-note note6) 12)
(check-expect (normalize-note note7) 2)
(check-expect (normalize-note note8) 12)
(check-expect (normalize-note note9) 1)
(check-expect (normalize-note note10) 4)
(check-expect (normalize-note note11) 2)
(check-expect (normalize-note note12) 3)
(check-expect (normalize-note note13) 6)
(check-expect (normalize-note note14) 4)
(check-expect (normalize-note note15) 5)
(check-expect (normalize-note note16) 7)
(check-expect (normalize-note note17) 5)
(check-expect (normalize-note note18) 6)
(check-expect (normalize-note note19) 9)
(check-expect (normalize-note note20) 7)
(check-expect (normalize-note note21) 8)


;; (normalize-note-list): transfer a list of note into
;; list of number
;; notmalize-note-list: listof Note -> listof Num
;; Examples:
(check-expect (normalize-note-list list2) (list 9 12 1 5 7))

(define (normalize-note-list list-of-note)
  (cond [(empty? list-of-note) empty]
        [else (cons (normalize-note (first list-of-note))
                    (normalize-note-list (rest list-of-note)))]))

;; Tests:
(check-expect (normalize-note-list list1) empty)
(check-expect (normalize-note-list list2) (list 9 12 1 5 7))
(check-expect (normalize-note-list list3) (list 1 2 12))

;; (interval note1 note2): calculate the distance between given
;; two note
;; intervel: Note1 Note2 -> Num
;; Examples: 
(check-expect (interval note9 note12) 2)
(check-expect (interval note12 note9) 10)

(define (interval note1 note2)
  (cond [(< (normalize-note note2)
            (normalize-note note1))
         (- (+ 12 (normalize-note note2))
            (normalize-note note1))]
        [else (- (normalize-note note2)
                 (normalize-note note1))]))
;; Tests:
(check-expect (interval note8 note8) 0)
(check-expect (interval note15 note3) 5)
(check-expect (interval note1 note10) 5)

;; (note-list-to-interval-list list-of-note): transfer a
;; list of note into a list of number by the distance of two notes
;; note-list-to-interval-list: listof Note -> listof Num 
;; example:
(check-expect (note-list-to-interval-list list2) (list 3 1 4 2 2))

(define (note-list-to-interval-list list-of-note)
  (cond [(empty? list-of-note) empty]
         [else (next
                (append list-of-note
                        (list (first list-of-note))))]))

(define (next list)
  (cond [(empty? (rest list)) empty]
        [else (cons (interval (first list) (second list))
                    (next (rest list)))]))

(check-expect (note-list-to-interval-list list1) empty)
(check-expect (note-list-to-interval-list list3) (list 1 10 1))


