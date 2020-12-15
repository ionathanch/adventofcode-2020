#lang curly-fn racket

(require "../lib.rkt")

(define input '(5 1 9 18 13 8 0))

(define (play end)
  (define turns (make-vector end #f))
  (for-each #{vector-set! turns %1 (add1 %2)}
            input (range (length input)))
  (let loop ([turn (length input)]
             [curr (last input)])
    (cond
      [(>= turn end) curr]
      [(vector-ref turns curr)
       (let ([next (- turn (vector-ref turns curr))])
         (vector-set! turns curr turn)
         (loop (add1 turn) next))]
      [else
       (vector-set! turns curr turn)
       (loop (add1 turn) 0)])))

(show-solution (play 2020) (play 30000000))
