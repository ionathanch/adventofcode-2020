#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input-grouped 22))

(define-values (player1 player2)
  #;(values '(9 2 6 3 1) '(5 8 4 7 10))
  (match input
    [(list p1 p2)
     (values (map string->number (rest (string-lines p1)))
             (map string->number (rest (string-lines p2))))]))

(define (combat player1 player2)
  (cond
    [(empty? player1) player2]
    [(empty? player2) player1]
    [else
     (let ([p1 (first player1)]
           [p2 (first player2)])
       (if (> p1 p2)
           (combat (append (rest player1) (list p1 p2))
                   (rest player2))
           (combat (rest player1)
                   (append (rest player2) (list p2 p1)))))]))

(define (recursive-combat player1 player2)
  (let loop ([player1 player1]
             [player2 player2]
             [seen (set)])
    (cond
      [(empty? player1) (list #f player2)]
      [(empty? player2) (list #t player1)]
      [(set-member? seen (list player1 player2)) (list #t player1)]
      [else
       (let ([p1 (first player1)]
             [p2 (first player2)]
             [seen (set-add seen (list player1 player2))])
         (cond
           [(and (<= p1 (sub1 (length player1)))
                 (<= p2 (sub1 (length player2))))
            (match-define (list p1-won? deck)
              (recursive-combat (take (rest player1) p1)
                                (take (rest player2) p2)))
            (if p1-won?
                (loop (append (rest player1) (list p1 p2))
                      (rest player2)
                      seen)
                (loop (rest player1)
                      (append (rest player2) (list p2 p1))
                      seen))]
           [(> p1 p2)
            (loop (append (rest player1) (list p1 p2))
                  (rest player2)
                  seen)]
           [(< p1 p2)
            (loop (rest player1)
                  (append (rest player2) (list p2 p1))
                  seen)]))])))

(define (score deck)
  (for/sum ([card deck]
            [score (reverse (range 1 (add1 (length deck))))])
    (* card score)))

(define part1
  (score (combat player1 player2)))

(define part2
  (score (second (recursive-combat player1 player2))))

(show-solution part1 part2)
