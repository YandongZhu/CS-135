;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname quaternion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;  Yandong Zhu (20588720)
;;  CS 135 Winter 2016
;;  Assignment 03, Problem 5
;;****************************************************************
;;

(define-struct quaternion (cc ic jc kc))
;; A Quaternion is a (make-quaternion Num Num Num Num)

;; my-quaternion: Quaternion -> Any
(define (my-quaternion q)
  ( ... (quaternion-cc q) ...
    ... (quaternion-ic q) ...
    ... (quaternion-jc q) ...
    ... (quaternion-kc q) ...))

;; Useful constant for examples and tests
(define q1 (make-quaternion 2 3 0 0))
(define q2 (make-quaternion 5 6 0 0))
(define q3 (make-quaternion 2 3 5 1))
(define q4 (make-quaternion 3 1 5 2))

;; (quat-mult a b q1 q2) multiply q1 and q2 by the given
;; constants a and b
;; quat-mult: Num Num Quaternion Quaternion -> Quaternion
;; Example:
(check-expect (quat-mult -1 -1 q1 q2) (make-quaternion -8 27 0 0))

(define (quat-mult a b q1 q2)
  (make-quaternion (+ (* (quaternion-cc q1) (quaternion-cc q2))
                      (* a (quaternion-ic q1) (quaternion-ic q2))
                      (* b (quaternion-jc q1) (quaternion-jc q2))
                      (* -1 a b (quaternion-kc q1) (quaternion-kc q1)))
                   (+ (* (quaternion-cc q1) (quaternion-ic q2))
                      (* (quaternion-ic q1) (quaternion-cc q2))
                      (* b (quaternion-kc q1) (quaternion-jc q2))
                      (* -1 b (quaternion-jc q1) (quaternion-kc q2)))
                   (+ (* (quaternion-cc q1) (quaternion-jc q2))
                      (* (quaternion-jc q1) (quaternion-cc q2))
                      (* a (quaternion-ic q1) (quaternion-kc q2))
                      (* -1 a (quaternion-kc q1) (quaternion-ic q2)))
                   (+ (* (quaternion-cc q1) (quaternion-kc q2))
                      (* (quaternion-kc q1) (quaternion-cc q2))
                      (* (quaternion-ic q1) (quaternion-jc q2))
                      (* (quaternion-jc q1) (quaternion-ic q2)))))

;; Tests
(check-expect (quat-mult 1 2 q3 q4) (make-quaternion 57 1 30 27))
(check-expect (quat-mult 1 1 q3 q1) (make-quaternion 12 12 7 17))
