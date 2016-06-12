;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname poker) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #t)))
(provide poker)

(define-struct poker (name point))

(define (poker-count lst)
  (local
    [(define (point-count poker)
       (cond
         [(empty? poker) 0]
         [(< (first poker) 8) 
          (+ (first poker) (point-count (rest poker)))]
         [(< (first poker) 10)
          (+ (* 2 (first poker)) (point-count (rest poker)))]
         [(< (first poker) 13)
          (+ (* 3 (first poker)) (point-count (rest poker)))]
         [(< (first poker) 14)
          (+ (* 4 (first poker)) (point-count (rest poker)))]))]
    (cond
      [(empty? lst) empty]
      [else (cons (make-poker (poker-name (first lst))
                              (point-count (poker-point (first lst))))
                  (poker-count (rest lst)))])))

(poker-count
 (list (make-poker 'Alex (list 9 3 11 2 5 1 7 7 7 4))
       (make-poker 'Janet (list 7 10 3 7 8 8 2 1 3 9))
       (make-poker 'CC (list 8 3 2 6 3))
       (make-poker 'rujia (list 6 5 4 9 4 1 5 7 6 10 2))))

(poker-count
 (list (make-poker 'Alex (list 9 4 1 6 10 5))
       (make-poker 'Janet (list 10 1 2 7 8 8 10))
       (make-poker 'CC (list 1 5 6 6))
       (make-poker 'RUJIA (list 9 3 1 3 6 6 3))))

(poker-count
 (list (make-poker 'Alex (list 2 3 8 1 7 1 3 2 7 1))
       (make-poker 'Janet (list 9 4 8 6 4 6 11 5 5 8 12 1))
       (make-poker 'CC (list 5 4 7 3))
       (make-poker 'RUJIA (list 4 2 6 7 5 3 4 2 6 4 13 7 2))))
