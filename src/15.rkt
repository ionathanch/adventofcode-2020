#lang curly-fn racket

(require "../lib.rkt")

(define input '(5 1 9 18 13 8 0))

(define (play end)
  (define turns
    (make-hash (map cons input (range 1 (add1 (length input))))))
  (let loop ([turn (length input)] [curr (last input)])
    (cond
      [(>= turn end) curr]
      [(hash-has-key? turns curr)
       (let ([next (- turn (hash-ref turns curr))])
         (hash-set! turns curr turn)
         (loop (add1 turn) next))]
      [else
       (hash-set! turns curr turn)
       (loop (add1 turn) 0)])))

(show-solution (play 2020) (play 30000000))
