;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname filedir) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;****************************************************************
;;    Yandong Zhu (20588720)
;;    CS 135 Wintr 2016
;;    Assignment 07, Problem 2
;;****************************************************************
;;

;; A FileSystem is a:
;; * Dir

;; A FileDir is one of:
;; * File
;; * Dir

;; A FDList is one of:
;; * empty
;; * (cons FileDir FDList)

;; (define-struct file (name size timestamp))
;; A File is a (make-file Str Nat Nat)

;; (define-struct dir (name contents))
;; A Dir is a (make-dir Str FDList)

(define file1 (make-file "oldfile" 1000 5))
(define file2 (make-file "newfile" 1000 55555555))
(define dir0 (make-dir "emptydir" empty))
(define dir1 (make-dir "onefile" (list file1)))
(define dir2 (make-dir "twofiles" (list file1 file2)))
(define dir2b (make-dir "twofiles" (list file1)))
(define dir3 (make-dir "onesies" (list dir1 dir1 dir1)))
(define fs1 (make-dir "rootdir" (list file1 dir0 dir1 dir2 dir3 file2)))
(define fs2 (make-dir "u" (list file2 dir0 dir1)))
(define fs3 (make-dir "a" (list (make-file "apple" 1000 10)
                                (make-dir "b" (list (make-file "b1" 104 8)
                                                    (make-file "b2" 389 2)
                                                    (make-file "b3" 3902 32)))
                                (make-dir "c" empty) 
                                (make-dir "d" (list
                                               (make-dir "e" (list (make-file "e1" 3920 19)
                                                                   (make-file "e2" 3924 15)))))
                                (make-file "a1" 3829 25))))

;; a
;; file
(define (my-file f)
  (... (file-name f)...
       ... (file-size f)...
       ... (file-timestamp f)... ))

;; dir
(define (my-dir d)
  (... (dir-name d)...
   ... (cond[(empty? (dir-contents d)) ...]
            [else ... (first (dir-contents d))
                  ... (rest (dir-contents d))])))

;; filedir
(define (my-filedir f)
  (cond [(file? f) ...]
        [else ...]))

;; FDlist
(define (my-fdlist f)
  (cond [(empty? f) ...]
        [(file? (first f))
         (...(first f)...(rest f)...)]
        [else (...(first f)...(rest f)...)]))

;; (b)
;; (count-files fs): count all the file number
;; in the file system
;; count-file: dir -> Int
;; Example:

;; (count-files fs): count all the file # in a filesystem
;; count-file: dir -> Int 
(check-expect (count-files dir1) 1)
(check-expect (count-files dir3) 3)

(define (count-files fs)
  (local
   [;; (count lst): count all the files in the
    ;; FDlist
    ;; count: (listof (anyof file dir)) -> Int
    (define (count lst)
      (cond
       [(empty? lst) 0]
       [(file? (first lst))
         (+ 1 (count (rest lst)))]
       [else (+ (count-files (first lst))
                (count (rest lst)))]))]
   (count (dir-contents fs))))

;; Tests
(check-expect (count-files dir0) 0)
(check-expect (count-files dir2) 2)
(check-expect (count-files fs1) 8)
(check-expect (count-files fs2) 2)
(check-expect (count-files fs3) 7)

;; (c)
;; (empty-dir-exist? fs): check all the dir in the
;; filesystem that have a non-empty contents or not
;; empty-dir-exist?: dir -> bool
;; Example:
(check-expect (empty-dir-exist? dir1) false)
(check-expect (empty-dir-exist? fs2) true)

(define (empty-dir-exist? fs)
  (local
    [;; (lst-deal lst): to check the dir contents
     ;; contain a empty-content dir or not
     ;; (lst-deal lst): (listof (anyof file dir)) -> bool
     (define (lst-deal lst)
       (cond 
         [(empty? lst) false]
         [(file? (first lst))
          (lst-deal (rest lst))]
         [else (or
                (empty-dir-exist? (first lst))
                (lst-deal (rest lst)))]))]
       (cond
         [(empty? (dir-contents fs)) true]
         [else (lst-deal (dir-contents fs))])))

;; Tests:
(check-expect (empty-dir-exist? dir0) true)
(check-expect (empty-dir-exist? fs3) true)
(check-expect (empty-dir-exist? dir3) false)


;; (d)
;; (oldest-file fs): to produce the file name that has
;; the smallest file timestamp
;; oldest-file: dir -> Str
;; requiries:
;; the file-system should have a non-empty content and
;; the first item of the content should be a file
;; Example:
(check-expect (oldest-file dir2) "oldfile")
(check-expect (oldest-file dir1) "oldfile")

(define (oldest-file fs) 
  (local 
    [;; (older file1 file2): to find the file has 
     ;; the smaller timestamp
     ;; older: file file -> file
     (define (older file1 file2)
       (cond
         [(> (file-timestamp file1)
             (file-timestamp file2)) file2]
         [else file1]))
     ;; (find lst file1): to compare the list of file
     ;; or dir and find the smallest timestamp file
     ;; find: find -> (listof (anyof file dir)) file -> file
     (define (find lst file1)
       (cond
         [(empty? lst) file1]
         [(file? (first lst))
          (find (rest lst)
                (older (first lst) file1))]
         [else (older (old (first lst) file1)
                      (find (rest lst) file1))]))
     ;; (old fs file): to find the oldest file in the dir
     ;; compare with the given file
     ;; old: dir file -> file
     (define (old fs file)
       (find (dir-contents fs) file))]        
    (file-name (old fs (first (dir-contents fs))))))

;; Tests
(check-expect (oldest-file fs3) "b2")




;; (d)
;; (list-file-paths): produce the list of string that
;; about each file including their hierarchical dir name
;; interval by "/"
;; (list-file-paths fs): dir -> (listof Str)
;; Example:
(check-expect (list-file-paths dir1) (list "onefile/oldfile"))
(check-expect (list-file-paths dir2) (list "twofiles/oldfile" "twofiles/newfile"))

(define (list-file-paths fs)
  (local
    [;; (lst-deal lst str): append the str onto the file
     ;; name and create a list about all the file in the list
     ;; lst-deal: FDlist Str -> listof Str
     (define (lst-deal lst str) 
      (cond
        [(empty? lst) empty]
        [(file? (first lst))
             (cons
              (string-append
               str
               (file-name (first lst)))
              (lst-deal (rest lst) str))]
        [else (append
               (deal-dir (first lst) str) 
               (lst-deal (rest lst) str))]))
     ;; (define (deal-dir dir st): append the dir 
     ;; name onto the given string and create a
     ;; list for the content of the dir
     ;; deal-dir: dir Str -> (listof Str)
      (define (deal-dir dir st)
      (lst-deal (dir-contents dir)
                (string-append st (dir-name dir) "/")))]
    (lst-deal (dir-contents fs)
              (string-append (dir-name fs) "/"))))

;; Tests
(check-expect (list-file-paths dir3) (list "onesies/onefile/oldfile"
                                           "onesies/onefile/oldfile"
                                           "onesies/onefile/oldfile"))
(check-expect (list-file-paths fs3) (list "a/apple"
                                          "a/b/b1"
                                          "a/b/b2"
                                          "a/b/b3"
                                          "a/d/e/e1"
                                          "a/d/e/e2"
                                          "a/a1"))

;; (f)
;; (backup-fs fs time1): create a new dir that
;; contain all the file that have the timestamp
;; less than the given time
;; backup-fs: dir Int -> dir
;; Example:
(check-expect (backup-fs dir1 2) (make-dir "onefile" empty))
(check-expect (backup-fs dir1 10) dir1)
(define (backup-fs fs time1)
  (local
    ;; create a FDlist that contain all the file
    ;; that which timestamp is less than the given time
    ;; deal-lst: FDlist Int -> FDlist
    [(define (deal-lst lst)
       (cond
         [(empty? lst) empty]
         [(file? (first lst))
          (cond
            [(< (file-timestamp (first lst)) time1)
             (cons
              (first lst)
              (deal-lst (rest lst)))]
            [else (deal-lst (rest lst))])]
         [else (cond
                 [(empty? (dir-contents (first lst)))
                  (deal-lst (rest lst))]
                 [else (cons 
                        (deal-dir (first lst))
                        (deal-lst (rest lst)))])]))
     ;; (deal-dir dir): make a dir for a non-empty dir contents
     ;; that delete the files that have bigger timestamp than the
     ;; given
     ;;deal-dir: dir -> dir
     (define (deal-dir dir)
      (make-dir (dir-name dir)
                (deal-lst (dir-contents dir))))]
    (cond [(empty? (dir-contents fs)) empty]
          [else (deal-dir fs)])))

;; Tests
(check-expect (backup-fs dir0 5) empty)
(check-expect (backup-fs dir2 10) (make-dir "twofiles" (list file1)))
(check-expect (backup-fs fs3 10) (make-dir "a" (list
                                                (make-dir "b" (list (make-file "b1" 104 8)
                                                                    (make-file "b2" 389 2)))
                                                (make-dir "d" (list (make-dir "e" empty))))))

(check-expect (backup-fs (make-dir "a" (list (make-file "a1" 1 1)
                                             (make-dir "b" empty)
                                             (make-file "c" 1 1))) 2) (make-dir "a" (list (make-file "a1" 1 1)
                                                                                          (make-file "c" 1 1))))
















