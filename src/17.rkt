#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map string->list (problem-input 17)))

(define grid
  (for*/fold ([grid (hash)])
             ([x (length (first input))]
              [y (length input)])
    (hash-set grid (list x y 0 0) (list-ref (list-ref input y) x))))

(define (actives chars)
  (count #{char=? % #\#} chars))

(define (cycle part1? grid)
  (define (in-range+ from to)
    (in-range (- from 1) (+ to 2)))
  (let* ([coords (hash-keys grid)]
         [xs (map first  coords)] [min-x (apply min xs)] [max-x (apply max xs)]
         [ys (map second coords)] [min-y (apply min ys)] [max-y (apply max ys)]
         [zs (map third  coords)] [min-z (apply min zs)] [max-z (apply max zs)]
         [ws (map fourth coords)] [min-w (apply min ws)] [max-w (apply max ws)])
    (for*/fold ([grid* grid])
               ([x (in-range+ min-x max-x)]
                [y (in-range+ min-y max-y)]
                [z (in-range+ min-z max-z)]
                [w (if part1? '(0) (in-range+ min-w max-w))])
      (let* ([cube (hash-ref grid (list x y z w) #\.)]
             [neighbours
              (for*/list ([x* (in-range+ x x)]
                          [y* (in-range+ y y)]
                          [z* (in-range+ z z)]
                          [w* (if part1? '(0) (in-range+ w w))]
                          #:unless (and (= x x*) (= y y*) (= z z*) (= w w*)))
                (hash-ref grid (list x* y* z* w*) #\.))]
             [active-neighbours (actives neighbours)])
        (cond
          [(and (char=? cube #\#) (not (<= 2 active-neighbours 3)))
           (hash-set grid* (list x y z w) #\.)]
          [(and (char=? cube #\.) (= active-neighbours 3))
           (hash-set grid* (list x y z w) #\#)]
          [else grid*])))))

(define part1
  (actives (hash-values ((iterate #{cycle #t} 6) grid))))

(define part2
  (actives (hash-values ((iterate #{cycle #f} 6) grid))))

(show-solution part1 part2)
