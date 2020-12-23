#lang curly-fn racket

(require "../lib.rkt")

(define input '(3 6 2 9 8 1 7 5 4))

(define CUPS 1000000)
(define MOVES 10000000)

(define cups
  (build-vector
   (add1 CUPS)
   #{cond
      [(or (= % 0) (= % CUPS)) (first input)]
      [(<= % (length input))
       (let ([i (index-of input %)])
         (if (< i (sub1 (length input)))
             (list-ref input (add1 i))
             (add1 (length input))))]
      [(< % CUPS) (add1 %)]}))

(define (play cup [cups cups] [CUPS CUPS])
  (define (prev cup)
    (if (= cup 1) CUPS (sub1 cup)))
  (let* ([one (vector-ref cups cup)]
         [two (vector-ref cups one)]
         [three (vector-ref cups two)]
         [four (vector-ref cups three)]
         [dest (let loop ([dest (prev cup)])
                 (if (or (= dest one) (= dest two) (= dest three))
                     (loop (prev dest))
                     dest))]
         [next (vector-ref cups dest)])
    (vector-set! cups cup four)
    (vector-set! cups three next)
    (vector-set! cups dest one)
    (vector-ref cups cup)))

(define part1
  (let ([cups (vector 0 7 9 6 3 4 2 5 1 8)])
    (let loop ([cup (first input)]
               [moves 0])
      (if (= moves 100)
          (let loop ([result '()] [cup 1])
            (define next (vector-ref cups cup))
            (if (= next 1)
                (apply string-append (map number->string (reverse result)))
                (loop (cons next result) next)))
          (loop (play cup cups (length input)) (add1 moves))))))

(define part2
  (let loop ([cup (first input)]
             [moves 0])
    (if (= moves MOVES)
        (let* ([one (vector-ref cups 1)]
               [two (vector-ref cups one)])
          (* one two))
        (loop (play cup) (add1 moves)))))

(show-solution part1 part2)
