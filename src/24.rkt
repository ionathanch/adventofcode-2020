#lang curly-fn racket

(require "../lib.rkt")

(define (parse path)
  (map string->symbol (regexp-match* #rx"(e|se|sw|w|nw|ne)" path)))

(define input (map parse (problem-input 24)))

(define (walk path)
  (for/fold ([x 0]
             [y 0]
             #:result (list x y))
            ([dir path])
    (match dir
      ['e  (values (add1 x) y)]
      ['w  (values (sub1 x) y)]
      ['se (values x (add1 y))]
      ['nw (values x (sub1 y))]
      ['ne (values (add1 x) (sub1 y))]
      ['sw (values (sub1 x) (add1 y))])))

(define (keep-flipped coords)
  (for/fold ([keep (set)])
            ([coord coords])
    (if (set-member? keep coord)
        (set-remove keep coord)
        (set-add keep coord))))

(define (neighbours coord)
  (match-let ([(list x y) coord])
    `((,(add1 x) ,y)
      (,(sub1 x) ,y)
      (,x ,(add1 y))
      (,x ,(sub1 y))
      (,(add1 x) ,(sub1 y))
      (,(sub1 x) ,(add1 y)))))

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
