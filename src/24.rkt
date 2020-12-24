#lang curly-fn racket

(require "../lib.rkt")

(define (parse path)
  (map string->symbol (regexp-match* #rx"(e|se|sw|w|nw|ne)" path)))

(define input (map parse (problem-input 24)))

(define (walk path)
  (let loop ([coord (list 0 0)]
             [path path])
    (if (empty? path)
        coord
        (match (list (first path) coord)
          [`(e  (,x ,y)) (loop (list (add1 x) y) (rest path))]
          [`(w  (,x ,y)) (loop (list (sub1 x) y) (rest path))]
          [`(se (,x ,y)) (loop (list x (add1 y)) (rest path))]
          [`(nw (,x ,y)) (loop (list x (sub1 y)) (rest path))]
          [`(ne (,x ,y)) (loop (list (add1 x) (sub1 y)) (rest path))]
          [`(sw (,x ,y)) (loop (list (sub1 x) (add1 y)) (rest path))]))))

(define (keep-flipped coords)
  (for/fold ([keep (set)])
            ([coord coords])
    (if (set-member? keep coord)
        (set-remove keep coord)
        (set-add keep coord))))

(define (neighbours coord)
  (match-let ([(list x y) coord])
    (list (list (add1 x) y) (list (sub1 x) y)
          (list x (add1 y)) (list x (sub1 y))
          (list (add1 x) (sub1 y)) (list (sub1 x) (add1 y)))))

(define (flip coords)
  (let* ([xs (set-map coords first)]  [min-x (sub1 (apply min xs))] [max-x (add1 (apply max xs))]
         [ys (set-map coords second)] [min-y (sub1 (apply min ys))] [max-y (add1 (apply max ys))])
    (for*/fold ([coords* coords])
               ([x (range min-x (add1 max-x))]
                [y (range min-y (add1 max-y))])
      (let* ([xy (list x y)]
             [bs (count #{set-member? coords %} (neighbours xy))]
             [black? (set-member? coords xy)])
        (cond
          [(and black? (not (<= 1 bs 2))) (set-remove coords* xy)]
          [(and (not black?) (= bs 2)) (set-add coords* xy)]
          [else coords*])))))

(define-values (part1 part2)
  (let* ([init (keep-flipped (map walk input))])
    (let loop ([flipped init] [days 0])
      (if (= days 100)
          (values (set-count init) (set-count flipped))
          (loop (flip flipped) (add1 days))))))

(show-solution part1 part2)
