;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (cs135-grade-sofar midterm assignment participation) (+ (*　0.5 midterm) (* 0.1 assignment) (* 0.4 participation)))

(define (cs135-final-exam exam grade) (+ (* 0.45 exam) (* 0.55 grade)))

