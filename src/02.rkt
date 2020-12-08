#lang racket

(require "../lib.rkt")

(define (parse line)
  (match-let ([(list min max char pass)
               (regexp-split #rx"[- ]" (regexp-replace #rx": " line " "))])
    (list (string->number min)
          (string->number max)
          (string-ref char 0)
          (string->list pass))))

(define input (map parse (problem-input 2)))

(define part1
  (count (match-lambda [(list min max char pass)
                        (<= min (count (λ~> (char=? char)) pass) max)])
         input))

(define part2
  (count (match-lambda [(list min max char pass)
                        (xor (char=? (list-ref pass (sub1 min)) char)
                             (char=? (list-ref pass (sub1 max)) char))])
         input))

(show-solution part1 part2)
