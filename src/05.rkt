#lang racket

(require "../lib.rkt")

(define (string->rc str)
  (let* ([row-str (substring str 0 7)]
         [col-str (substring str 7 10)]
         [as-binary (∘ string->number (∂ string-append "#b"))]
         [row (as-binary (regexp-replaces row-str '([#rx"F" "0"] [#rx"B" "1"])))]
         [col (as-binary (regexp-replaces col-str '([#rx"L" "0"] [#rx"R" "1"])))])
    (cons row col)))

(define input (map string->rc (problem-input 5)))

(define part1
  (apply max (map (match-lambda [`(,row . ,col) (+ (* row 8) col)]) input)))

(define part2
  (let ([grid (make-vector-grid 8 128 #\o)])
    (for ([rc input])
      (vector-grid-update grid rc #\x))
    (let ([str (apply string-append (map list->string (vectors->lists grid)))])
      (add1 (car (first (regexp-match-positions #rx"xox" str)))))))

(show-solution part1 part2)
