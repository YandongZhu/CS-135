;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname area) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 03, Problem 3
;;****************************************************************
;;

(define-struct 3dposn (x y z))
;; A 3dposn is a (make-3dposn Num Num Num)
;;my-3dposn: 3dposn -> Num
(define (my-3dposn d)
  ( ... (3dposn-x d) ...
    ... (3dposn-y d) ...
    ... (3dposn-z d) ...))

;; Useful value for examples and test
(define a (make-3dposn 0 4 0))
(define b (make-3dposn 3 0 0))
(define c (make-3dposn 4 0 0))
(define f (make-3dposn 0 0 0))
(define d (make-3dposn 5 8 5))
(define h (make-3dposn -1 0 1))
(define i (make-3dposn 2 4 3))

;; (distance 3dposn 3dposn2) to calculate the distance between
;; two point
;; distance: 3dposn 3dposn -> Num
;; Example:
(check-expect (distance a b) 5)

(define (distance 3dposn1 3dposn2)
  (sqrt (+ (sqr (- (3dposn-x 3dposn1) (3dposn-x 3dposn2)))
           (sqr (- (3dposn-y 3dposn1) (3dposn-y 3dposn2)))
           (sqr (- (3dposn-z 3dposn1) (3dposn-z 3dposn2))))))

;; Tests:
(check-expect (distance a b) 5)
(check-expect (distance b c) 1)

;; (scale point factor) scale point by given factor
;; scale: 3dposn Num -> 3dposn
;; Example:
(check-expect (scale a 2) (make-3dposn 0 2 0))

(define (scale point factor)
  (make-3dposn (/ (3dposn-x point) factor)
               (/ (3dposn-y point) factor)
               (/ (3dposn-z point) factor)))
;; Tests:
(check-expect (scale a 2) (make-3dposn 0 2 0))
(check-expect (scale b 2) (make-3dposn 1.5 0 0))

;; (3dposn=? 3dposn1 3dposn2) judge two point is same or not 
;; 3dposn=?: 3dposn 3dposn -> Bool
;; Example:
(check-expect (3dposn=? a b) false)

(define (3dposn=? 3dposn1 3dposn2)
  (cond [(and (= (3dposn-x 3dposn1) (3dposn-x 3dposn2))
              (= (3dposn-y 3dposn1) (3dposn-y 3dposn2))
              (= (3dposn-z 3dposn1) (3dposn-z 3dposn2))) true]
        [else false]))

;; Tests:
(check-expect (3dposn=? a a) true)
(check-expect (3dposn=? b c) false)

;; (area-triangle a b c) to check three point is co-linear
;; or not. If it is, then produces 'undefined, otherwise
;; produces the triangle's area create by the three
;; points
;; area-triangle: 3dposn 3dposn 3dposn -> (Anyof Sym Num)
;; Example:
(check-expect (area-triangle a b f) 6)

(define (area-triangle a b c)
  (cond [(or (= 0 (gcd (- (3dposn-x b) (3dposn-x a))
                       (- (3dposn-y b) (3dposn-y a))
                       (- (3dposn-z b) (3dposn-z a))))
             (= 0 (gcd (- (3dposn-x c) (3dposn-x a))
                       (- (3dposn-y c) (3dposn-y a))
                       (- (3dposn-z c) (3dposn-z a))))
             (= 0 (gcd (- (3dposn-x c) (3dposn-x b))
                       (- (3dposn-y c) (3dposn-y b))
                       (- (3dposn-z c) (3dposn-z b))))) 'undefined]
        [(or (3dposn=?
              (scale
                (make-3dposn (- (3dposn-x b) (3dposn-x a))
                             (- (3dposn-y b) (3dposn-y a))
                             (- (3dposn-z b) (3dposn-z a)))
                (gcd (- (3dposn-x b) (3dposn-x a))
                     (- (3dposn-y b) (3dposn-y a))
                     (- (3dposn-z b) (3dposn-z a))))
              (scale
                (make-3dposn (- (3dposn-x c) (3dposn-x b))
                             (- (3dposn-y c) (3dposn-y b))
                             (- (3dposn-z c) (3dposn-z b)))
                (gcd (- (3dposn-x c) (3dposn-x b))
                     (- (3dposn-y c) (3dposn-y b))
                     (- (3dposn-z c) (3dposn-z b)))))
             (3dposn=?
               (scale
                (make-3dposn (- (3dposn-x b) (3dposn-x a))
                             (- (3dposn-y b) (3dposn-y a))
                             (- (3dposn-z b) (3dposn-z a)))
                (gcd (- (3dposn-x b) (3dposn-x a))
                     (- (3dposn-y b) (3dposn-y a))
                     (- (3dposn-z b) (3dposn-z a))))
               (scale
                (scale
                 (make-3dposn (- (3dposn-x c) (3dposn-x b))
                              (- (3dposn-y c) (3dposn-y b))
                              (- (3dposn-z c) (3dposn-z b)))
                 (gcd (- (3dposn-x c) (3dposn-x b))
                      (- (3dposn-y c) (3dposn-y b))
                      (- (3dposn-z c) (3dposn-z b)))) -1))) 'undefined]
        [else (* 1/4 (sqrt (- (+ (* 2 (sqr (distance a c))
                                      (sqr (distance a b)))
                                 (* 2 (sqr (distance a b))
                                      (sqr (distance b c)))
                                 (* 2 (sqr (distance b c))
                                      (sqr (distance a c))))
                              (expt (distance b c) 4)
                              (expt (distance a c) 4)
                              (expt (distance a b) 4))))]))

;;Tests:
(check-expect (area-triangle a a f) 'undefined)
(check-expect (area-triangle b b f) 'undefined)
(check-expect (area-triangle i d h) 'undefined)
(check-expect (area-triangle f d d) 'undefined)
(check-expect (area-triangle a b f) 6)
(check-within (area-triangle a c f) 8 0.5)
