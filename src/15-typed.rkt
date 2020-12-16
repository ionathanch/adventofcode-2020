#lang typed/racket

(require/typed "../lib.rkt"
               [show-solution (-> Any Any Void)])

(: input (Listof Natural))
(define input '(5 1 9 18 13 8 0))

(: play (-> Exact-Positive-Integer Integer))
(define (play end)
  (let ([turns : (Vectorof Natural) (make-vector end 0)])
    (for-each (λ ([i : Natural] [j : Natural]) (vector-set! turns i (add1 j)))
              input (range (length input)))
    (let loop ([turn : Natural (length input)]
               [curr : Integer (last input)])
      (cond
        [(>= turn end) curr]
        [(positive? (vector-ref turns curr))
         (let ([next : Integer (- turn (vector-ref turns curr))])
           (vector-set! turns curr turn)
           (loop (add1 turn) next))]
        [else
         (vector-set! turns curr turn)
         (loop (add1 turn) 0)]))))

(show-solution (play 2020) (play 30000000))
