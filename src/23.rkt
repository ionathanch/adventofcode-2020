#lang curly-fn racket

(require "../lib.rkt")

(define input '(3 6 2 9 8 1 7 5 4))

(define (succ idx)
  (modulo (add1 idx) 9))

(define (add1* c)
  (if (= c 9) 1 (add1 c)))

(define (sub1* c)
  (if (= c 1) 9 (sub1 c)))

(define (play current cups moves stop)
  (if (= moves stop)
      cups
      (let* ([curr-idx (index-of cups current)]
             [picked-up (list (list-ref cups (succ curr-idx))
                              (list-ref cups (succ (succ curr-idx)))
                              (list-ref cups (succ (succ (succ curr-idx)))))]
             [dest-idx (let loop ([c (sub1* current)])
                         (if (member c picked-up)
                             (loop (sub1* c))
                             (index-of cups c)))])
        (define-values (front back) (split-at cups (+ dest-idx 1)))
        (let ([cups (append (remove* picked-up front) picked-up (remove* picked-up back))])
          (play (list-ref cups (succ (index-of cups current))) cups (add1 moves) stop)))))

(define part1
  (let* ([cups (play (first input) input 0 100)]
         [i (index-of cups 1)]
         [labels (append (drop cups (succ i)) (take cups i))])
    (apply string-append (map number->string labels))))

(define part2 #f)

(show-solution part1 part2)
