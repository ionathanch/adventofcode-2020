#lang curly-fn racket

(require "../lib.rkt")

(define input (map string-lines (problem-input-grouped 20)))

(define (parse-tile tile-strings)
  (match-let ([`(,name ,rows ...) tile-strings])
    (list (string->number (second (regexp-match #px"Tile (\\d+):" name)))
          (map string->list rows))))

(define tiles
  (map parse-tile input))

(define (tile-edges tile)
  (match-let ([(list name rows) tile])
    (list name (list (first rows)
                     (map last rows)
                     (reverse (last rows))
                     (reverse (map first rows))))))

(define tiles-edges
  (map tile-edges tiles))

(define (corner? edges tiles)
  (let ([tiles (map second tiles)])
    (= 2 (count #{for/or ([target tiles])
                   (or (member % target)
                       (member (reverse %) target))}
                edges))))

(define part1
  (for/product ([tile tiles-edges]
                #:when (corner? (second tile)
                                (remove tile tiles-edges)))
    (first tile)))

(define part2 #f)

(show-solution part1 part2)
