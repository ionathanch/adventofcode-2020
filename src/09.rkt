#lang racket

(require "../lib.rkt")

(define input (map string->number (problem-input 9)))
(define input* (list->vector input))

(define (pair-sum start end)
  (for*/or ([i (range start end)]
            [j (range start end)])
    (= (vector-ref input* end)
       (+ (vector-ref input* i)
          (vector-ref input* j)))))

(define part1
  (let loop ([start 0] [end 25])
    (if (pair-sum start end)
        (loop (add1 start) (add1 end))
        (vector-ref input* end))))

(define part2
  (let loop ([start 0] [end 0] [sum 0])
    (cond
      [(< sum part1)
       (loop start (add1 end) (+ sum (vector-ref input* end)))]
      [(> sum part1)
       (loop (add1 start) end (- sum (vector-ref input* start)))]
      [else (let ([contig (drop (take input end) start)])
              (+ (apply min contig) (apply max contig)))])))

(show-solution part1 part2)
