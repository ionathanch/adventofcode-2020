#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 9)))

(define (pair-sum lst target)
  (for*/or ([i lst]
            [j lst])
    (= (+ i j) target)))

(define part1
  (let loop ([current (take input 25)] [next (list-ref input 25)] [remain (drop input 26)])
    (if (pair-sum current next)
        (loop (snoc (rest current) next) (first remain) (rest remain))
        next)))

(define part2
  (let loop ([current '()] [remain input] [sum 0])
    (cond
      [(< sum part1)
       (loop (snoc current (first remain)) (rest remain) (+ sum (first remain)))]
      [(> sum part1)
       (loop (rest current) remain (- sum (first current)))]
      [else (+ (apply min current) (apply max current))])))

(show-solution part1 part2)
