;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pizza) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (pizza-price symbol number number symbol)
;; first symbol belongs to {'small, 'medium, 'large}
;; second symbol belongs to {'half-off, 'big-eater, 'supersize, 'solo}
(define small 6)
(define medium 8)
(define large 9.5)
(define stardandprice 1)
(define premiumprice 1.5)
(define (pizza-price size stardand premium coupon)
  (cond [(symbol=? coupon 'half-off)
        (cond [(symbol=? size 'small)
              (/ (+ small
                  (* stardandprice stardand)
                  (* premiumprice premium)) 2)]
              [(symbol=? size 'medium)
              (/ (+ medium
                  (* stardandprice stardand)
                  (* premiumprice premium)) 2)]
              [(symbol=? size 'large)
              (/ (+ large
                  (* stardandprice stardand)
                  (* premiumprice premium)) 2)])]
        [(symbol=? coupon 'big-eater) 18]
        [(symbol=? coupon 'supersize)
         (+ small
            (* stardandprice stardand)
            (* premiumprice premium))]
        [(and (symbol=? coupon 'solo)
              (symbol=? size 'small)
              (= premium 2)
              (= stardand 0)) 8]
        [else (cond [(symbol=? size 'small)
                 (+ small
                    (* stardandprice stardand)
                    (* premiumprice premium))]
                [(symbol=? size 'medium)
                 (+ medium
                    (* stardandprice stardand)
                    (* premiumprice premium))]
                [(symbol=? size 'large)
                 (+ large
                    (* stardandprice stardand)
                    (* premiumprice premium))])]))

