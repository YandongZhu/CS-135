;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bstbonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (define-struct node (key val left right))

;; (bst-nth bst int): to find the nth smallest
;; key in the bst and produce its correspond value
;; if it is not exist, then produce false
;; bst-nth: bst Int -> (anyof Bool Str)
(define (bst-nth bst int)
  (local
    [;; (elminate-left bst): to elminate the smallest
     ;; element in the bst
     ;; elminate-left: bst -> bst 
     (define (elminate-left bst)
       (cond
         [(and
           (empty? (node-left bst))
           (empty? (node-right bst))) empty]
         [(empty? (node-left bst))
          (node-right bst)]
         [(empty? (node-left (node-left bst)))
          (make-node
           (node-key bst)
           (node-val bst)
           (node-right (node-left bst))
           (node-right bst))]
         [else (make-node
                (node-key bst)
                (node-val bst)
                (elminate-left (node-left bst))
                (node-right bst))]))]
    (cond
      [(empty? bst) false]
      [(= 1 int) (cond [(empty? (node-left bst)) (node-val bst)]
                       [else (bst-nth (node-left bst) int)])]
      [else (bst-nth (elminate-left bst)(sub1 int))])))

