#lang racket

(require racket/set
         "../lib.rkt")

(define (parse group)
  (map (∘ list->set string->list) (string-split group "\n")))

(define input (map parse (problem-input-grouped 6)))

(define-values (part1 part2)
  (let* ([any-counts (map (∘ set-count ($ set-union)) input)]
         [all-counts (map (∘ set-count ($ set-intersect)) input)])
    (values (sum any-counts) (sum all-counts))))

(show-solution part1 part2)
