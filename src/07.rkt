#lang racket

(require (rename-in (except-in graph transpose)
                    [get-neighbors get-neighbours])
         "../lib.rkt")

(define input (problem-input 7))

(define (strings->graphs bags)
  (let ([contained (weighted-graph/directed '())]
        [contains (weighted-graph/directed '())])
    (for ([bag bags])
      (match-let ([(list _ outer inners) (regexp-match #rx"^(.+) bags contain (.+).$" bag)])
        (for ([inner (string-split inners ",")])
          (match inner
            [(regexp #px"(\\d+) (.+) bags?" (list _ count inner))
             (add-directed-edge! contained inner outer (string->number count))
             (add-directed-edge! contains outer inner (string->number count))]
            [else (add-vertex! contained outer)
                  (add-vertex! contains outer)]))))
    (values contained contains)))

(define (count-bags graph source multiplier)
  (let ([neighbours (get-neighbours graph source)])
    (* (apply + 1 (map (λ (neighbour)
                         (let ([weight (edge-weight graph source neighbour)])
                           (count-bags graph neighbour weight)))
                       neighbours))
       multiplier)))

(define-values (part1 part2)
  (let*-values ([(contained contains) (strings->graphs input)]
                [(bfs-shiny-gold pred) (bfs contained "shiny gold")]
                [(reachables) (count (∘ (∂ != +inf.0) cdr) (hash->list bfs-shiny-gold))]
                [(total-bags) (count-bags contains "shiny gold" 1)])
    (values (sub1 reachables) (sub1 total-bags))))

(show-solution part1 part2)
