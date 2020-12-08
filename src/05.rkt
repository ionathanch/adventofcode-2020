#lang racket

(require racket/set
         "../lib.rkt")

(define (string->seat str)
  (~>> str
      (regexp-replaces _ '([#rx"F" "0"] [#rx"B" "1"] [#rx"L" "0"] [#rx"R" "1"]))
      (string-append "#b")
      string->number))

(define input (map string->seat (problem-input 5)))

(define-values (part1 part2)
  (let* ([minimum (apply min input)]
         [maximum (apply max input)]
         [seats (list->set (range minimum (add1 maximum)))]
         [filled (list->set input)])
    (values maximum (set-first (set-subtract seats filled)))))

(show-solution part1 part2)
