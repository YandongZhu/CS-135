;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (m/s->mph x) (/ x (* (/ 1 3600) 1609.344)))

(define (fpf->mph x) (/ (* (/ 1.8288 1609.344) x) (* 14 24)))

(define (mph->S/nc x) (/ (/ 1609.344 1.7018) (/ 3600 3.15576)))
