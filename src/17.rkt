#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map string->list (problem-input 17)))

(define grid
  (for*/fold ([grid (hash)])
             ([x 8]
              [y 8])
    (hash-set grid (list x y 0 0) (list-ref (list-ref input y) x))))

(define (actives chars)
  (count #{char=? % #\#} chars))

(define (cycle part1? grid)
  (let* ([coords (hash-keys grid)]
         [xs (map first  coords)] [min-x (apply min xs)] [max-x (apply max xs)]
         [ys (map second coords)] [min-y (apply min ys)] [max-y (apply max ys)]
         [zs (map third  coords)] [min-z (apply min zs)] [max-z (apply max zs)]
         [ws (map fourth coords)] [min-w (apply min ws)] [max-w (apply max ws)])
    (for*/fold ([grid* grid])
               ([x (in-range (- min-x 1) (+ max-x 2))]
                [y (in-range (- min-y 1) (+ max-y 2))]
                [z (in-range (- min-z 1) (+ max-z 2))]
                [w (if part1? '(0) (in-range (- min-w 1) (+ max-w 2)))])
      (let* ([cube (hash-ref grid (list x y z w) #\.)]
             [neighbours (for*/list ([x* (in-range (- x 1) (+ x 2))]
                                     [y* (in-range (- y 1) (+ y 2))]
                                     [z* (in-range (- z 1) (+ z 2))]
                                     [w* (if part1? '(0) (in-range (- w 1) (+ w 2)))]
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
