#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input 3))

(define (trees grid right down)
  (define width (string-length (list-ref grid 0)))
  (define height (length grid))
  (count #{char=? #\#
                  (string-ref (list-ref grid %)
                              (modulo (* (/ % down) right) width))}
         (range 0 height down)))

(define part1
  (trees input 3 1))

(define part2
  (* (trees input 1 1)
     (trees input 3 1)
     (trees input 5 1)
     (trees input 7 1)
     (trees input 1 2)))

(show-solution part1 part2)
