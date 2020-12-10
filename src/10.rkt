#lang curly-fn racket

(require "../lib.rkt")

(define input (sort (map string->number (problem-input 10)) <=))

(define part1
  (let* ([input (cons 0 input)]
         [diffs (for/list ([i (rest input)] [j input]) (- i j))]
         [ones (count #{= % 1} diffs)]
         [threes (add1 (count #{= % 3} diffs))])
    (* ones threes)))

(define arrangements
  (for*/fold ([arrs (hash 0 1)])
             ([joltage input]
              [i (range 1 4)])
    (let ([prev (hash-ref arrs (- joltage i) 0)])
      (hash-update arrs joltage #{+ % prev} 0))))

(define part2
  (hash-ref arrangements (last input)))

(show-solution part1 part2)
