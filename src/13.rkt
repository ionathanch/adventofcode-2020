#lang curly-fn racket

(require math/number-theory
         "../lib.rkt")

(define input (problem-input 13))
(define timestamp (string->number (first input)))
(define busses (map string->number (string-split (second input) ",")))
(define ids (filter identity busses))

(define part1
  (let* ([waits (map #{- (* % (add1 (quotient timestamp %))) timestamp} ids)]
         [wait (apply min waits)]
         [index (index-of waits wait)])
    (* wait (list-ref ids index))))

(define part2
  (let ([offsets (filter-map #{and %1 (negate %2)} busses (range (length busses)))])
    (solve-chinese offsets ids)))

(show-solution part1 part2)
