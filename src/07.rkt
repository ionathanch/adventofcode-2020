#lang racket

(require (rename-in (except-in graph transpose)
                    [get-neighbors get-neighbours])
         "../lib.rkt")

(define (parse bag)
  (match-let* ([(list _ outer inners) (regexp-match #rx"^(.+) bags contain (.+)$" bag)]
               [inner-list (map (∘ (∂r string-trim " bag")
                                   (∂r string-trim " bags"))
                                (string-split (string-trim inners ".") ", "))]
               [inner-bags (filter-map (match-lambda
                                         [(regexp #px"(\\d+) (.+)" (list _ count style))
                                          (cons (string->number count) style)]
                                         [else #f])
                                       inner-list)])
    (cons outer inner-bags)))

(define input (map parse (problem-input 7)))

(define (containment-graphs bags)
  (let ([contained (weighted-graph/directed '())]
        [contains (weighted-graph/directed '())])
    (for ([bag bags])
      (match-let ([(list outer inners ...) bag])
        (for ([inner inners])
          (match-let ([(cons count style) inner])
            (add-directed-edge! contained style outer count)
            (add-directed-edge! contains outer style count)))))
    (values contained contains)))

(define (count-bags graph source multiplier)
  (let ([neighbours (get-neighbours graph source)])
    (* (apply + 1 (map (λ (neighbour)
                         (let ([weight (edge-weight graph source neighbour)])
                           (count-bags graph neighbour weight)))
                       neighbours))
       multiplier)))

(define-values (part1 part2)
  (let*-values ([(contained contains) (containment-graphs input)]
                [(bfs-shiny-gold pred) (bfs contained "shiny gold")]
                [(reachables) (count (∘ (∂ != +inf.0) cdr) (hash->list bfs-shiny-gold))]
                [(total-bags) (count-bags contains "shiny gold" 1)])
    (values (sub1 reachables) (sub1 total-bags))))

(show-solution part1 part2)
