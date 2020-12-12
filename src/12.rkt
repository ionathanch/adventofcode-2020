#lang curly-fn racket

(require threading
         "../lib.rkt")

(define (parse line)
  (match line
    [(regexp #px"([NESWLRF])(\\d+)" (list _ dir amt))
     (list (string->symbol dir) (string->number amt))]))

(define input (map parse (problem-input 12)))

(define (turn curr-dir dir angle)
  (define dirs
    (if (symbol=? dir 'R)
        '(N E S W)
        '(N W S E)))
  (define index (index-of dirs curr-dir))
  (list-ref dirs (modulo (+ index (/ angle 90)) 4)))

(define (rotate x y dir amt)
  (cond
    [(= amt 180) (list (- x) (- y))]
    [(= amt 270) (rotate x y (if (symbol=? dir 'R) 'L 'R) 90)]
    [(symbol=? dir 'R) (list (- y) x)]
    [(symbol=? dir 'L) (list y (- x))]))

(define (move instr x y dir)
  (match instr
    [`(N ,amt) (values x (- y amt) dir)]
    [`(E ,amt) (values (+ x amt) y dir)]
    [`(S ,amt) (values x (+ y amt) dir)]
    [`(W ,amt) (values (- x amt) y dir)]
    [`(F ,amt) (move `(,dir ,amt) x y dir)]
    [`(,lr ,amt) (values x y (turn dir lr amt))]))

(define (move* instr x-ship y-ship x-wpt y-wpt)
  (match instr
    [`(N ,amt) (values x-ship y-ship x-wpt (- y-wpt amt))]
    [`(E ,amt) (values x-ship y-ship (+ x-wpt amt) y-wpt)]
    [`(S ,amt) (values x-ship y-ship x-wpt (+ y-wpt amt))]
    [`(W ,amt) (values x-ship y-ship (- x-wpt amt) y-wpt)]
    [`(F ,amt) (values (+ x-ship (* x-wpt amt)) (+ y-ship (* y-wpt amt)) x-wpt y-wpt)]
    [`(,lr ,amt)
     (match-let ([(list x-wpt y-wpt) (rotate x-wpt y-wpt lr amt)])
       (values x-ship y-ship x-wpt y-wpt))]))

(define (part1)
  (define-values (x y dir)
    (for/fold ([x 0]
               [y 0]
               [dir 'E])
              ([instr input])
      (move instr x y dir)))
  (+ (abs x) (abs y)))

(define (part2)
  (define-values (x-ship y-ship x-wpt y-wpt)
    (for/fold ([x-ship 0]
               [y-ship 0]
               [x-wpt 10]
               [y-wpt -1])
              ([instr input])
      (move* instr x-ship y-ship x-wpt y-wpt)))
  (+ (abs x-ship) (abs y-ship)))

(show-solution (part1) (part2))
