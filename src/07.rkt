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

(define (count-bags graph source)
  (let ([neighbours (get-neighbours graph source)])
    (add1 (sum (map (λ (neighbour)
                      (let ([weight (edge-weight graph source neighbour)])
                        (* weight (count-bags graph neighbour))))
                    neighbours)))))

(define-values (part1 part2)
  (let*-values ([(contained contains) (strings->graphs input)]
                [(bfs-shiny-gold pred) (bfs contained "shiny gold")]
                [(reachables) (count (λ~> cdr (!= +inf.0)) (hash->list bfs-shiny-gold))]
                [(total-bags) (count-bags contains "shiny gold")])
    (values (sub1 reachables) (sub1 total-bags))))

(show-solution part1 part2)
