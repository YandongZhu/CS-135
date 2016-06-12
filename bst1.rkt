;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 07, Problem 1
;;****************************************************************
;;

;; A binary search tree (BST) is one of:
;; * empty
;; * a Node
;; (define-struct node (key val left right))

;; Useful constants for example and test
(define bst1 (make-node 100 "100" empty empty))
(define bst2 (make-node 100 "100"
                        (make-node 90 "90"
                                   (make-node 80 "80"
                                              (make-node 70 "70"
                                                         (make-node 60 "60" empty empty)
                                                         (make-node 75 "75" empty empty)) empty)
                                   (make-node 95 "95"
                                              (make-node 93 "110" empty empty)
                                              (make-node 99 "99" empty empty)))
                        (make-node 110 "110"
                                   (make-node 105 "105" empty empty)
                                   (make-node 120 "120"
                                              (make-node 115 "115" empty empty)
                                              (make-node 130 "130" empty
                                                         (make-node 140 "120" empty empty))))))
;; A Node is a (make-node Num Str BST BST)
;; requires: key > every key in left BST
;; key < every key in right BST

;; (a)
;; (bst-internal-count bst): count all the internal
;; node within a binary search tree
;; bst-internal-count: bst -> Int
;; Examples:
(check-expect (bst-internal-count empty) 0)
(check-expect (bst-internal-count bst2) 8)



(define (bst-internal-count bst)
	(cond
          [(empty? bst) 0]
          [(and
            (empty? (node-left bst))
            (empty? (node-right bst))) 0]
	  [else (+ 1 (bst-internal-count
                      (node-right bst))
                   (bst-internal-count
                    (node-left bst)))]))
;; Tests:
(check-expect (bst-internal-count bst1) 0)



;; (b)
;; (bst-bounded? bst low high): to check the
;; max and min # in the bst is within the
;; boundary of the low and high# or not
;; bst-bounded?: bst Int Int -> Bool
;; requires: low <= high
;; Examples:
(check-expect (bst-bounded? bst2 0 200) true)
(check-expect (bst-bounded? bst2 50 100) false)

(define (bst-bounded? bst low high)
  (local
    [;; (min-node bst) to find the smallest key in the bst
     ;; min-node: bst -> Int
     (define (min-node bst)
       (cond
         [(empty? (node-left bst))
          (node-key bst)]
         [else (min-node (node-left bst))]))
     ;; (max-node bst) to find the biggest key in the bst
     ;; max-node: bst-> Int
     (define (max-node bst)
       (cond
         [(empty? (node-right bst))
          (node-key bst)]
        [else (max-node (node-right bst))]))]
    (cond [(empty? bst) true]
          [else (and
                 (<= low (min-node bst))
                 (>= high (max-node bst)))])))

;; Tests:
(check-expect (bst-bounded? empty 5 6) true)
(check-expect (bst-bounded? bst1 99 101) true)
(check-expect (bst-bounded? bst2 60 130) false)
(check-expect (bst-bounded? bst2 70 140) false)
(check-expect (bst-bounded? bst2 60 140) true)
(check-expect (bst-bounded? bst2 61 139) false)





;; (c)
;; (bst-add bst key value): to insert a key and
;; value into a bst
;; bst-add: bst Int Str -> bst
;; Example:
(check-expect (bst-add bst1 100 "110") (make-node 100 "110" empty empty))
(check-expect (bst-add bst1 110 "110") (make-node 100 "100" empty
                                                  (make-node 110 "110" empty empty)))

(define (bst-add bst key value)
  (cond
    [(empty? bst) (make-node key value empty empty)]
    [(= key (node-key bst))
     (make-node
      (node-key bst)
      value
      (node-left bst)
      (node-right bst))]
    [(< key (node-key bst))
     (make-node
      (node-key bst)
      (node-val bst)
      (bst-add (node-left bst) key value)
      (node-right bst))]
    [(> key (node-key bst))
     (make-node
      (node-key bst)
      (node-val bst)
      (node-left bst)
      (bst-add (node-right bst) key value))]))

;; Tests:
(check-expect (bst-add empty 100 "110") (make-node 100 "110" empty empty))
(check-expect (bst-add bst1 90 "90") (make-node 100 "100"
                                                (make-node 90 "90" empty empty)
                                                empty))
(check-expect (bst-add bst2 97 "97")
(make-node 100 "100"
           (make-node 90 "90"
                      (make-node 80 "80"
                                 (make-node 70 "70"
                                            (make-node 60 "60" empty empty)
                                            (make-node 75 "75" empty empty)) empty)
                      (make-node 95 "95"
                                 (make-node 93 "110" empty empty)
                                 (make-node 99 "99"
                                            (make-node 97 "97" empty empty)empty)))
           (make-node 110 "110"
                      (make-node 105 "105" empty empty)
                      (make-node 120 "120"
                                 (make-node 115 "115" empty empty)
                                 (make-node 130 "130" empty
                                            (make-node 140 "120" empty empty))))))
    
   



;; (d)
;; (bst-> al bst): create a (listof (listof key value))
;; to sort all the value in the bst by key ascending order
;; bst->al: bst -> (listof (listof Int Str))
;; Example:
(check-expect (bst->al bst1) (list (list 100 "100")))
(check-expect (bst->al empty) empty)

(define (bst->al bst)
  (cond
    [(empty? bst) empty]
    [else (append
           (bst->al (node-left bst))
           (list
            (list
             (node-key bst)
             (node-val bst)))
           (bst->al (node-right bst)))]))

;; Tests:
(check-expect (bst->al (make-node 100 "100"
                                  (make-node 90 "90" empty empty)
                                  empty)) (list (list 90 "90") (list 100 "100")))
(check-expect (bst->al bst2) (list (list 60 "60") (list 70 "70") (list 75 "75") (list 80 "80") (list 90 "90") (list 93 "110")
                                   (list 95 "95") (list 99 "99") (list 100 "100") (list 105 "105") (list 110 "110") (list 115 "115") 
                                   (list 120 "120") (list 130 "130") (list 140 "120")))



;; (e)
;; (bst-value-list bst): to create a list of string
;; corresponding the value of the bst by key
;; decreasing order. the list is no duliplicate
;; and will keep the one with smaller key
;; bst-value-list: bst -> listof Str
;; Example:
(check-expect (bst-value-list bst1) (list "100"))
(check-expect (bst-value-list (make-node 100 "100"
                                    (make-node 90 "90" empty empty) empty)) (list "100" "90"))

(define (bst-value-list bst)
  (local [;; (exist str ls): to add the string onto
          ;; the last list. if the string already exist
          ;; in the list, delete the orginal one and add
          ;; the new one.
          ;; exist: str (listof Str) -> (listof Str)
          (define (exist str ls)
            (cond
              [(empty? ls) (list str)]
              [(string=? str (first ls))
               (append (rest ls)
                       (list str))]
              [else (cons
                     (first ls)
                     (exist str (rest ls)))]))
          ;; create a list of str by given bst
          ;; corresponding key by decreasing order
          ;; create-str: bst bst (listof Str) -> listof Str
          (define (create-str bst1 bst2 ls)
            (cond
              [(empty? bst1) ls]
              [(empty? (node-right bst2))
               (create-str
                (elminate-right bst1)
                (elminate-right bst1)
                (exist (node-val bst2) ls))]
              [else (create-str
                     bst1
                     (node-right bst2)
                     ls)]))
          ;; elminate the element which has the biggest key
          ;; elminate-right: bst -> bst
          (define (elminate-right bst)
            (cond
              [(empty? bst) empty]
              [(and
                (empty? (node-left bst))
                (empty? (node-right bst))) empty]
              [(empty? (node-right bst))
               (node-left bst)]
              [(empty? (node-right (node-right bst)))
               (make-node
                (node-key bst)
                (node-val bst)
                (node-left bst)
                (node-left (node-right bst)))]
              [else (make-node
                     (node-key bst)
                     (node-val bst)
                     (node-left bst)
                     (elminate-right (node-right bst)))]))]
    (create-str bst bst empty)))

;; Test
(check-expect (bst-value-list empty) empty)
(check-expect (bst-value-list bst2) (list "130" "120" "115" "105" "100" "99" "95" "110" "90" "80" "75" "70" "60"))









