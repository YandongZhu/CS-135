;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname aaaa10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct pos (x y))
;; A Pos is a (make-pos Nat Nat)
;; A Grid is a (listof (listof Char))
;; requires: both inner and outer lists of Grid are non-empty
(define-struct state (puzzle pieces))
;; A State is a (make-state Grid (listof Grid))

(define (lists-equiv? l1 l2)
  ;; The approach is a bit sneaky, but very succinct.  Check that
  ;; every element of l1 appears somewhere in l2 (in terms of equal?),
  ;; and that every elements of l2 appears somewhere in l1.
  (and (= (length l1) (length l2))
       (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
       (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2)))

;; Useful constants for test and example
(define grid1 (list
               (list #\q #\q)
               (list #\q #\q)
               (list #\q #\q)))

(define grid2 (list
               (list #\a #\a)
               (list #\a #\a)
               (list #\a #\.)))

(define grid3 (list
               (list #\a #\a #\a)
               (list #\. #\a #\.)
               (list #\. #\a #\.)))

(define grid4 (list
               (list #\. #\a #\.)
               (list #\a #\a #\a)
               (list #\. #\a #\.)))

(define grid5 (list
               (list #\. #\. #\.)
               (list #\. #\. #\.)
               (list #\. #\. #\.)))

;; a1
;; build a 2d plane by given width height and function
;; build-2dlist: Nat Nat (Num Num -> X) -> (listof (listof X))
;; requires: the given function should be able to consume two
;; varibles
;; Example:
(check-expect (build-2dlist 2 3 make-pos) (list
                                           (list (make-pos 0 0) (make-pos 1 0))
                                           (list (make-pos 0 1) (make-pos 1 1))
                                           (list (make-pos 0 2) (make-pos 1 2))))

(check-expect (build-2dlist 2 2 (lambda (x y)
                                  #\.))
              '((#\. #\.)
                (#\. #\.)))


(define (build-2dlist width height f)
  (build-list height (lambda (x)
                       (build-list width (lambda (y)
                                           (f y x))))))

;; Tests:
(check-expect (build-2dlist 0 0 -) empty)

(check-expect (build-2dlist 5 6 +)
              '((0 1 2 3 4)
                (1 2 3 4 5)
                (2 3 4 5 6)
                (3 4 5 6 7)
                (4 5 6 7 8)
                (5 6 7 8 9)))




;; a2
;; select all the position by given width and height
;; all-positions: Nat Nat -> (listof pos)
;; Example: 
(check-expect (lists-equiv?
               (all-positions 2 3)
               (list (make-pos 0 0) (make-pos 1 0)
                     (make-pos 0 1) (make-pos 1 1)
                     (make-pos 0 2) (make-pos 1 2)))
              true)

(check-expect (lists-equiv?
               (all-positions 2 2)
               (list (make-pos 0 0)(make-pos 1 0)
                     (make-pos 0 1)(make-pos 1 1)))
              true)

(define (all-positions width height)
  (foldr append empty (build-2dlist width height make-pos)))

;;Tests
(check-expect (all-positions 0 0) empty)

(check-expect (lists-equiv?
               (all-positions 2 2)
               (list (make-pos 0 0)(make-pos 1 0)(make-pos 2 0)
                     (make-pos 0 1)(make-pos 1 1)))
              false)


;; b
;; create all the possible shape of grid a given polyomino
;; all-orientationbie: grid -> (listof grid)
;; Example:
(check-expect (lists-equiv?
               (all-orientations grid1)
               '(((#\q #\q)
                  (#\q #\q)
                  (#\q #\q))
                 ((#\q #\q #\q)
                  (#\q #\q #\q)))) true)

(check-expect (lists-equiv?
               (all-orientations grid4)
               '(((#\. #\a #\.)
                  (#\a #\a #\a)
                  (#\. #\a #\.)))) true)

(define (all-orientations grid)
  (local
    [;; turn the given grid by 90 degree
     ;; crosswise: grid -> grid
     (define (crosswise grid)
       (cond
         [(empty? (first grid)) empty]
         [else (cons (map first grid)
                     (crosswise (map rest grid)))]))
     ;; create the mirror structure by given grid
     (define mirror
       (map reverse grid))
     ;; create the upward-mirror structure by given grid
     (define upward-mirror
       (reverse grid))
     ;; create the upware structure by given grid
     (define upward
       (map reverse upward-mirror))
     ;; create the left-turn-mirror structure by given grid
     (define left-turn-mirror
       (crosswise grid))
     ;; create the left-turn structure by given grid
     (define left-turn
       (map reverse left-turn-mirror))
     ;; create the right-turn structure by given grid
     (define right-turn
       (reverse left-turn-mirror))
     ;; create the right-turn-mirror structure by given grid
     (define right-turn-mirror
       (reverse left-turn))
     ;; create all 8 kinds of structures by given grid
     (define listof-all
       (list grid mirror upward-mirror upward left-turn-mirror
             left-turn right-turn right-turn-mirror))]
    (foldr (lambda (x y)
             (cond [(not (member? x y)) (cons x y)]
                   [else y])) empty listof-all)))

;;Tests
(check-expect (lists-equiv?
               (all-orientations grid3)
               '(((#\a #\a #\a)
                  (#\. #\a #\.)
                  (#\. #\a #\.))
                ((#\. #\. #\a)
                 (#\a #\a #\a)
                 (#\. #\. #\a))
                ((#\. #\a #\.)
                 (#\. #\a #\.)
                 (#\a #\a #\a))
                ((#\a #\. #\.)
                 (#\a #\a #\a)
                 (#\a #\. #\.))))
              true)

(check-expect (lists-equiv?
               (all-orientations grid2)
               '(((#\. #\a)
                  (#\a #\a)
                  (#\a #\a))
                 ((#\a #\a)
                  (#\a #\a)
                  (#\a #\.))
                 ((#\a #\.)
                  (#\a #\a)
                  (#\a #\a))
                 ((#\a #\a)
                  (#\a #\a)
                  (#\. #\a))
                 ((#\a #\a #\a)
                  (#\a #\a #\.))
                 ((#\a #\a #\.)
                  (#\a #\a #\a))
                 ((#\. #\a #\a)
                  (#\a #\a #\a))
                 ((#\a #\a #\a)
                  (#\. #\a #\a)))) true)

;; c
;; (first-empty-pos grid): find the first empty point by given grid,
;; if all full, produce fasle
;; first-empty-pos: grid -> (anyof pos false)
;; Example:
(check-expect (first-empty-pos grid2) (make-pos 1 2))
(check-expect (first-empty-pos grid3) (make-pos 0 1))

(define (first-empty-pos grid)
  (local 
    [;; create the list of all the position
     (define list-pos
       (all-positions (length (first grid)) (length grid)))
     ;; create the list of all the element in grid
     (define list-grid
       (foldr append empty grid))
     ;; (find-pos list-grid list-pos): find the first #\. grid
     ;; and return the correspond pos or false
     ;; find-pos: (listof pos) (listof grid) -> (anyof pos false)
     (define (find-pos list-grid list-pos)
       (cond
         [(empty? list-grid) false]
         [(char=? (first list-grid) #\.) (first list-pos)]
         [else (find-pos (rest list-grid) (rest list-pos))]))]
    (find-pos list-grid list-pos)))

(check-expect (first-empty-pos grid1) false)
(check-expect (first-empty-pos grid4)
              (make-pos 0 0))

;; d
;; (superimpose base cover pos): superimpose the cover grid onto the base
;; by given pos
;; superimopose: grid grid pos -> grid
;; requires:
;; the pos should can be found in the base grid
;; Example:
(check-expect (superimpose grid5 grid1 (make-pos 0 0))
              '((#\q #\q #\.)
                (#\q #\q #\.)
                (#\q #\q #\.)))

(check-expect (superimpose grid3 grid4 (make-pos 0 0))
              '((#\a #\a #\a)
                (#\a #\a #\a)
                (#\. #\a #\.)))

(define (superimpose base cover pos)
  (local 
    [;; the x coordinate of the given pos
     (define row (pos-x pos))
     ;; the y coordinate of the given pos
     (define column (pos-y pos))
     ;; (find-column base cover column): find the correct column 
     ;; to insert the cover grid
     ;; find-column: grid grid Nat -> grid
     (define (find-column base cover column)
       (cond
         [(empty? base) empty]
         [(empty? cover) base]
         [(= 0 column)
          (cons
           (find-row (first base) (first cover) row)
           (find-column (rest base) (rest cover) column))]
         [else (cons
                (first base)
                (find-column (rest base) cover (sub1 column)))]))
     ;; (find-row base-row cover-row row): find the correct row to insert
     ;; the cover grid row
     ;; find-row: (listof char) (listof char) Nat -> (listof char)
     (define (find-row base-row cover-row row)
       (cond
         [(empty? base-row) empty]
         [(= 0 row) (substitute base-row cover-row)]
         [else (cons (first base-row)
                     (find-row (rest base-row) cover-row (sub1 row)))]))
     ;; (substitute base-row cover-row): substitude the base-row by cover-row
     ;; substitute: (listof char) (listof char) -> (listof char)
     (define (substitute base-row cover-row)
       (cond
         [(empty? base-row) empty]
         [(empty? cover-row) base-row]
         [(char=? #\. (first cover-row))
          (cons (first base-row)
                (substitute (rest base-row) (rest cover-row)))]
         [else (cons (first cover-row)
                     (substitute (rest base-row) (rest cover-row)))]))]
    (find-column base cover column))) 

(check-expect (superimpose '((#\b #\. #\. #\. #\.)
                             (#\b #\b #\b #\. #\.)
                             (#\. #\b #\. #\. #\.))
                           '((#\a #\a)
                             (#\a #\a)
                             (#\a #\.))
                           (make-pos 0 0))
              '((#\a #\a #\. #\. #\.)
                (#\a #\a #\b #\. #\.)
                (#\a #\b #\. #\. #\.)))

(check-expect (superimpose '((#\b #\. #\. #\. #\.)
                             (#\b #\b #\b #\. #\.)
                             (#\. #\b #\. #\. #\.))
                           '((#\a #\a)
                             (#\a #\a)
                             (#\a #\.))
                           (make-pos 3 1))
              '((#\b #\. #\. #\. #\.)
                (#\b #\b #\b #\a #\a)
                (#\. #\b #\. #\a #\a)))

;; e
;; (fit-in puzzle grid pos): check a polyomion can fit in the
;; the first blank or not 
;; fit-in: grid grid pos -> bool
;; Example:
(check-expect (fit-in grid5 grid4 (make-pos 0 0)) true)
(check-expect (fit-in grid5 grid4 (make-pos 3 3)) false)

(define (fit-in puzzle grid pos)
  (local
    [;; the x coordinater of the pos 
     (define row (pos-x pos))
     ;; the y coordinater of the pos
     (define column (pos-y pos))
     ;; (find-column puzzle grid column): find the column of
     ;; the puzzle that grid can be inserted or not
     ;; find-column: grid grid Nat -> bool
     (define (find-column puzzle grid collumn)
       (cond
         [(empty? grid) true]
         [(empty? puzzle) false]
         [(= 0 column)
          (and
           (find-row (first puzzle) (first grid) row)
           (find-column (rest puzzle) (rest grid) column))]
         [else (find-column (rest puzzle) grid (sub1 column))]))
     ;; (find-row puzzle-row grid-row row): find the grid-row can fit-in
     ;; the puzzle-row or not
     ;; find-row: (listof Char) (listof Char) Nat -> bool
     (define (find-row puzzle-row grid-row row)
       (cond
         [(= 0 row) (check puzzle-row grid-row)]
         [else (find-row (rest puzzle-row) grid-row (sub1 row))]))
     ;; (check puzzle-row grid-row):
     ;; check: (listof Char) (listfo Char) -> bool
     (define (check puzzle-row grid-row)
       (cond
         [(empty? grid-row) true]
         [(empty? puzzle-row) false]
         [(and
           (not (char=? #\. (first puzzle-row)))
           (not (char=? #\. (first grid-row)))) false]
         [else (check (rest puzzle-row) (rest grid-row))]))]
    (find-column puzzle grid column)))

;; Tests
(check-expect (fit-in grid3 grid2 (make-pos 0 0)) false)
(check-expect (fit-in grid2 grid3 (make-pos 0 0)) false)
(check-expect (fit-in grid4 grid3 (make-pos 0 0)) false)

;; (neighbours state): produce a listof state that the pieces in
;; the state can legally fit in the puzzle and produce its neighbours
;; state of the original state
;; neighbours: state -> (listof state)
(define (neighbours state) 
  (local
    [;; the pieces list
     (define puzzle-list (state-pieces state))
     ;; the puzzle map
     (define puzzle (state-puzzle state))
     ;; the list length
     (define lth (length puzzle-list))
     ;; the first blank on the puzzle
     (define first-blank (first-empty-pos puzzle))
     ;; (neighbours-state puzzle listof-poly listof-piece):
     ;; create a neighbour state list if the polyomino can fit in the
     ;; first blank point of the puzzle
     ;; neighbours-state: grid (listof grid) (listof grid): (listof state)
     (define (neighbours-state puzzle listof-poly listof-piece)
       (cond 
         [(empty? listof-poly) empty]
         [(fit-in puzzle (first listof-poly) first-blank)
          (cons (make-state
                 (superimpose puzzle (first listof-poly) first-blank)
                 listof-piece)
                (neighbours-state puzzle (rest listof-poly) listof-piece))]
         [else (neighbours-state puzzle (rest listof-poly) listof-piece)]))
     ;; (put-on puzzle puzzle-list lth): create the list of state
     ;; that is able to fit
     ;; put-on: grid (listof grid) Nat -> (listof state)
     (define (put-on puzzle puzzle-list lth)
       (cond  
         [(= 0 lth) empty]
         [else (append (neighbours-state puzzle
                                         (all-orientations (first puzzle-list))
                                         (rest puzzle-list))
                       (put-on puzzle (append
                                       (rest puzzle-list)
                                       (list (first puzzle-list))) (sub1 lth)))]))]
    (cond  
      [(false? first-blank) empty]
      [else (put-on puzzle puzzle-list lth)])))

;(neighbours (make-state '((#\a #\.) (#\. #\a))
;'(((#\X #\X)))))


