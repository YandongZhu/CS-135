;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (q1a x)
  (cond [(and (p1? x)
              (p2? x)) 'up]
        [(and (p1? x)
              (not (p2? x))) 'down]
        [(p2? x) 'jump]
        [(p3? x) 'left]
        [else 'right]))

(define (q1b x)
  (cond [(and (p1? x) (< x 2016)) 'heart]
        [(and (p1? x) (p2? x)) 'spade]
        [(and (not (p1? x)) (p3? x)) 'club]
        [else 'diamond]))

(define (q1c x)
  (cond [(and (p1? x)
              (p2? x)
              (p3? x))
         'alpha]
        [(and (p1? x)
              (p2? x))
         'bravo]
        [else 'charlie]))

