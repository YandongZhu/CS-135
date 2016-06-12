;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname bst) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 08, Problem 1
;;****************************************************************
;;

(define-struct node (key val left right))

;; Useful constant for examples and tests


 
;; a
;; (root-at-smallest bst): to move the smallest 
;; key in a bst to the root
;; root-at-small: bst -> bst
;; Example:
(check-expect (root-at-smallest (make-node 100 "100"
                                           (make-node 90 "90" empty empty)
                                           (make-node 110 "110" empty empty)))
            (make-node 90 "90" empty 
                       (make-node 100 "100" empty
                                  (make-node 110 "110" empty empty))))
(check-expect (root-at-smallest (make-node 90 "90" empty
                                           (make-node 100 "100" empty empty)))
              (make-node 90 "90" empty
                         (make-node 100 "100" empty empty)))

(define (root-at-smallest bst)
    (cond
      [(empty? bst) empty] 
      [(empty? (node-left bst)) bst]
      [else (local
              ;; smallest-so-far is the current smaller bst compare than the
              ;; the root
            [(define smallest-so-far (root-at-smallest (node-left bst)))]
              (make-node
               (node-key smallest-so-far)
               (node-val smallest-so-far)
               empty
               (make-node
                (node-key bst)
                (node-val bst)
                (node-right smallest-so-far)
                (node-right bst))))]))

;; Tests
(check-expect (root-at-smallest empty) empty)
(check-expect (root-at-smallest (make-node 100 "100" empty empty))
              (make-node 100 "100" empty empty))
(check-expect (root-at-smallest (make-node 5 "5"
                                           (make-node 3 "3" empty empty) empty))
              (make-node 3 "3" empty (make-node 5 "5" empty empty)))

(check-expect (root-at-smallest (make-node '4 "4"
                                           (make-node '2 "2"
                                                      (make-node '1 "1" empty empty)
                                                      (make-node '3 "3" empty empty))
                                           (make-node '6 "6" empty empty)))
              (make-node '1 "1" empty
                         (make-node '4 "4"
                                    (make-node '2 "2" empty
                                               (make-node '3 "3" empty empty))
                                    (make-node '6 "6" empty empty)))) 





;; b
;; (bst-remove bst key)to remove the bst key 
;; and value which equal to the given key
;; bst-remove: bst Num -> bst
;; example:
(check-expect (bst-remove 100 (make-node 100 "100"
                                         (make-node 90 "90" empty empty)
                                         (make-node 110 "110" empty empty)))
              (make-node 110 "110"
                         (make-node 90 "90" empty empty) empty))
(check-expect (bst-remove 120 (make-node 100 "100"
                                         (make-node 90 "90" empty empty)
                                         (make-node 110 "110" empty empty)))
               (make-node 100 "100"
                         (make-node 90 "90" empty empty)
                         (make-node 110 "110" empty empty)))


(define (bst-remove key bst)
  (local
    ;; (equal bst1): to deal with the different situations 
    ;; when the key is equal to the given key
    ;; (equal: bst -> bst
    [(define (equal bst1)  
       (cond
         [(empty? (node-left bst1)) (node-right bst1)]
         [(empty? (node-right bst1)) (node-left bst1)]
         [else (make-node
                (node-key (root-at-smallest (node-right bst1)))
                (node-val (root-at-smallest (node-right bst1)))
                (node-left bst1)
                (node-right (root-at-smallest (node-right bst1))))]))]
    (cond
      [(empty? bst) empty]
      [(= key (node-key bst))
       (equal bst)]
      [(> key (node-key bst))
       (make-node
        (node-key bst)
        (node-val bst)
        (node-left bst)
        (bst-remove key (node-right bst)))]
      [(< key (node-key bst))
       (make-node
        (node-key bst)
        (node-val bst)
        (bst-remove key (node-left bst))
        (node-right bst))])))

;; Tests:
(check-expect (bst-remove 100 empty) empty)
(check-expect (bst-remove 100 (make-node 100 "100" empty empty)) empty)
(check-expect (bst-remove 100 (make-node 100 "100"
                                         (make-node 90 "90" empty empty) empty))
              (make-node 90 "90" empty empty))
(check-expect (bst-remove 100 (make-node 100 "100" empty
                                         (make-node 110 "110" empty empty)))
              (make-node 110 "110" empty empty))
(check-expect (bst-remove 2 (make-node 2 "2"
                                       (make-node 1 "1" empty empty)
                                       (make-node 5 "5"
                                                  (make-node 3 "3" empty empty) empty)))
              (make-node 3 "3" (make-node 1 "1" empty empty) (make-node 5 "5" empty empty)))

(check-expect (bst-remove 4 (make-node 2 "2"
                                       (make-node 1 "1" empty empty)
                                       (make-node 8 "5"
                                                  (make-node 3 "3" empty  empty) empty)))
              (make-node 2 "2"
                         (make-node 1 "1" empty empty)
                         (make-node 8 "5"
                                    (make-node 3 "3" empty  empty) empty)))

 
