;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname rpsls) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; (rpsls symbol symbol)
;; symbol belongs to {'rock,'paper, 'scissors, 'lizard, 'spock}
(define (rpsls player1 player2)
  (cond
    [(or (and (symbol=? player1 'rock) (or (symbol=? player2 'paper)
                                           (symbol=? player2 'spock)))
         (and (symbol=? player1 'paper) (or (symbol=? player2 'scissors)
                                            (symbol=? player2 'lizard)))
         (and (symbol=? player1 'scissors) (or (symbol=? player2 'rock)
                                            (symbol=? player2 'spock)))
         (and (symbol=? player1 'lizard) (or (symbol=? player2 'rock)
                                            (symbol=? player2 'scissors)))
         (and (symbol=? player1 'spock) (or (symbol=? player2 'paper)
                                            (symbol=? player2 'lizard))))
     'player2]
    [(or (and (symbol=? player1 'rock) (symbol=? player2 'rock))
         (and (symbol=? player1 'paper) (symbol=? player2 'paper))
         (and (symbol=? player1 'spock) (symbol=? player2 'spcok))
         (and (symbol=? player1 'lizard) (symbol=? player2 'lizard))
         (and (symbol=? player1 'scissors) (symbol=? player2 'scissors)))
     'tie]
    [else 'player1]))
    
   

