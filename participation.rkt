;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname participation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (cs135-participation total correct incorrect) (* (/ (+ (* 2 (min correct (* 0.75 total))) (* 1 (min incorrect (max 0 (- (* 0.75 total) correct))))) (* total 0.75 2)) 100))

