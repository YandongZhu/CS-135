;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define pass 49)
(define participationpercent 0.05)
(define assignmentpercent 0.2)
(define midtermpercent 0.25)
(define finalpercent 0.5)
(define limit 46)
(define (cs135-final-grade midterm participation assignment final)
  (cond [(and (> (/ (+ midterm final) 2) pass) (> assignment pass))
         (+ (* participationpercent participation)
            (* assignmentpercent assignment)
            (* midtermpercent midterm)
            (* finalpercent final))]
        [else (min (+ (* participationpercent participation)
                      (* assignmentpercent assignment)
                      (* midtermpercent midterm)
                      (* finalpercent final)) limit)]))

