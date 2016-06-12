;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname plants) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;(a)
(define-struct cityinfo (name zone subzone))
;; a cityinfo is a (make-cityinfo Str Nat Sym)
;; requires:
;; 0 <= zone <= 9
;; subzone is one of ('a, 'b)

;;(b)
(define-struct plantinfo (name zone subzone))
;; a plantinfo is a (make-plantinfo Str Nat Sym)
;; requires:
;; 0 <= zone <= 9
;; subzone is one of ('a, 'b)

;; Useful constant for example and tests
(define plantinfo1 (make-plantinfo "blue eyed grass" 5 'b))
(define plantinfo2 (make-plantinfo "hosta" 3 'a))
(define plantinfo3 (make-plantinfo "columbine" 4 'a))
(define plantinfo4 (make-plantinfo "chrysanthemum" 3 'b))
(define plantinfo5 (make-plantinfo "toad lily" 4 'b))
(define plantinfo6 (make-plantinfo "agapanthus" 8 'a))
(define plantinfo7 (make-plantinfo "liriope" 7 'b))
(define sample-plant-data (list plantinfo1 plantinfo2 plantinfo3 plantinfo4 plantinfo5 plantinfo6 plantinfo7))
(define cityinfo1 (make-cityinfo "Vancouver" 8 'b))
(define cityinfo2 (make-cityinfo "Edmonton" 3 'a))
(define cityinfo3 (make-cityinfo "Waterloo" 5 'b))
(define cityinfo4 (make-cityinfo "Saint John" 5 'a))
(define cityinfo5 (make-cityinfo "Halifax" 6 'a))
(define cityinfo6 (make-cityinfo "Happy Valley-Goose Bay" 1 'a))
(define sample-city-data (list cityinfo1 cityinfo2 cityinfo3 cityinfo4 cityinfo5 cityinfo6))

;; (c)
;; (find-hardy-plant cityinfo list-of-plantinfo): select a list of
;; plantinfo that can survive in the given city
;; find-hardy-plant: cityinfo, listof plantinfo -> listof plant info
;; examples:
(check-expect (find-hardy-plants cityinfo2 sample-plant-data) (list plantinfo2))

(define (find-hardy-plants cityinfo list-of-plantinfo)
  (cond [(empty? list-of-plantinfo) empty]
        [(> (cityinfo-zone cityinfo)
            (plantinfo-zone (first list-of-plantinfo)))
         (cons (first list-of-plantinfo)
               (find-hardy-plants cityinfo (rest list-of-plantinfo)))]
        [(= (cityinfo-zone cityinfo)
            (plantinfo-zone (first list-of-plantinfo)))
         (cond [(or (symbol=? (plantinfo-subzone (first list-of-plantinfo)) 'a)
                    (symbol=? 'b
                              (cityinfo-subzone cityinfo)))
                (cons (first list-of-plantinfo)
                      (find-hardy-plants cityinfo (rest list-of-plantinfo)))]
               [else (find-hardy-plants cityinfo (rest list-of-plantinfo))])]
        [else (find-hardy-plants cityinfo (rest list-of-plantinfo))]))
;; Tests:
(check-expect (find-hardy-plants cityinfo1 sample-plant-data) (list plantinfo1  plantinfo2  plantinfo3  plantinfo4  plantinfo5  plantinfo6  plantinfo7))
(check-expect (find-hardy-plants cityinfo3 sample-plant-data) (list plantinfo1  plantinfo2  plantinfo3  plantinfo4  plantinfo5))
(check-expect (find-hardy-plants cityinfo4 sample-plant-data) (list plantinfo2  plantinfo3  plantinfo4  plantinfo5))


;; (d)
;; (find-growing-cities plantinfo list-of-city): select a list of
;; city that can survive in the given plant
;; find-growing-cities: plantinfo, listof cityinfo -> listof Str
;; examples:
(check-expect (find-growing-cities plantinfo2 sample-city-data) (list "Vancouver" "Edmonton" "Waterloo" "Saint John" "Halifax"))

(define (find-growing-cities plantinfo list-of-city)
  (cond [(empty? list-of-city) empty]
        [(< (plantinfo-zone plantinfo)
            (cityinfo-zone (first list-of-city)))
         (cons (cityinfo-name (first list-of-city))
               (find-growing-cities plantinfo (rest list-of-city)))]
        [(= (plantinfo-zone plantinfo)
            (cityinfo-zone (first list-of-city)))
         (cond [(or (symbol=? (plantinfo-subzone plantinfo) 'a)
                    (symbol=? 'b
                              (cityinfo-subzone (first list-of-city))))
                (cons (cityinfo-name (first list-of-city))
                      (find-growing-cities plantinfo (rest list-of-city)))]
               [else (find-growing-cities plantinfo (rest list-of-city))])]
        [else (find-growing-cities plantinfo (rest list-of-city))]))

;; Tests
(check-expect (find-growing-cities plantinfo1 sample-city-data) (list "Vancouver" "Waterloo" "Halifax"))
(check-expect (find-growing-cities plantinfo4 sample-city-data) (list "Vancouver" "Waterloo" "Saint John" "Halifax"))
(check-expect (find-growing-cities plantinfo5 sample-city-data) (list "Vancouver" "Waterloo" "Saint John" "Halifax"))

(define-struct fuck (name plant))


;;(e)
(define (create-list citylist plantlist)
  (cond [(empty? citylist) empty]
        [else (cons (make-fuck
                     (cityinfo-name (first citylist))
                     (find-hardy-plants (first citylist) plantlist))
                     (create-list (rest citylist) plantlist))]))

(define (select list)
  (cond [(empty? list) empty]
        [else (cond [(empty? (fuck-plant (first list))) (cons (fuck-name (first list)) (select (rest list)))]
                    [else (select (rest list))])]))

(define (wth citylist plantlist)
  (select (create-list citylist plantlist)))

(wth sample-city-data sample-plant-data)


