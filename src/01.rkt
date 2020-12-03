#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 1)))

(define part1
  (let loop ([entries input]
             [ht (hash)])
    (let ([entry (first entries)])
      (if (hash-has-key? ht entry)
          (* entry (hash-ref ht entry))
          (loop (rest entries) (hash-set ht (- 2020 entry) entry))))))

(define part2
  (for*/first ([i input]
               [j (cdr input)]
               [k (cddr input)]
               #:when (= (+ i j k) 2020))
    (* i j k)))

(show-solution part1 part2)
