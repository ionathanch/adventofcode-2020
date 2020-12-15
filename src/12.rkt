#lang racket

(require "../lib.rkt")

(define (parse line)
  (match line
    [(regexp #px"([NESWLRF])(\\d+)" (list _ dir amt))
     (list (string->symbol dir) (string->number amt))]))

(define input (map parse (problem-input 12)))

(define (move instr p1 p2 x y dx dy)
  (match instr
    [`(N ,amt) (values x (- y (* amt p1)) dx (- dy (* amt p2)))]
    [`(E ,amt) (values (+ x (* amt p1)) y (+ dx (* amt p2)) dy)]
    [`(S ,amt) (values x (+ y (* amt p1)) dx (+ dy (* amt p2)))]
    [`(W ,amt) (values (- x (* amt p1)) y (- dx (* amt p2)) dy)]
    [`(F ,amt) (values (+ x (* dx amt)) (+ y (* dy amt)) dx dy)]
    [(or '(R 90) '(L 270)) (values x y (- dy) dx)]
    [(or '(L 90) '(R 270)) (values x y dy (- dx))]
    [`(,_ 180) (values x y (- dx) (- dy))]))

(define part1
  (for/fold ([x  0] [y  0]
             [dx 1] [dy 0]
             #:result (+ (abs x) (abs y)))
            ([instr input])
    (move instr 1 0 x y dx dy)))

(define part2
  (for/fold ([x-ship 0] [y-ship 0]
             [x-wpt 10] [y-wpt -1]
             #:result (+ (abs x-ship) (abs y-ship)))
            ([instr input])
    (move instr 0 1 x-ship y-ship x-wpt y-wpt)))

(show-solution part1 part2)
