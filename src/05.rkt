#lang racket

(require racket/set
         "../lib.rkt")

(define (string->seat str)
  (string->number (string-append "#b" (regexp-replaces str '([#rx"F" "0"] [#rx"B" "1"] [#rx"L" "0"] [#rx"R" "1"])))))

(define input (map string->seat (problem-input 5)))

(define-values (part1 part2)
  (let* ([minimum ($ min input)]
         [maximum ($ max input)]
         [seats (list->set (range minimum (add1 maximum)))]
         [filled (list->set input)])
    (values maximum (set-first (set-subtract seats filled)))))

(show-solution part1 part2)
