#lang curly-fn racket

(require (for-syntax racket/syntax
                     [only-in racket/list
                              range
                              make-list])
         "../lib.rkt")

(define input
  (map string->list (problem-input 17)))

(define (grid dim)
  (for*/fold ([grid (hash)])
             ([x (length (first input))]
              [y (length input)])
    (hash-set grid
              `(,x ,y ,@(make-list (- dim 2) 0))
              (list-ref (list-ref input y) x))))

(define (actives chars)
  (count #{char=? % #\#} chars))

(define (in-range+ from to)
  (in-range (- from 1) (+ to 2)))

(define-syntax cycle
  (λ (stx)
    (syntax-case stx ()
      [(_ dim grid)
       (let ([dim (syntax->datum #'dim)])
         (with-syntax* ([(i     ...) (range dim)]
                        [(d     ...) (generate-temporaries (make-list dim 'd))]
                        [(ds    ...) (generate-temporaries (make-list dim 'ds))]
                        [(d-min ...) (generate-temporaries (make-list dim 'd-min))]
                        [(d-max ...) (generate-temporaries (make-list dim 'd-max))]
                        [(d*    ...) (generate-temporaries (make-list dim 'd*))])
           #'(let* ([coords (hash-keys grid)]
                    [ds (map #{list-ref % i} coords)] ...
                    [d-min (apply min ds)] ...
                    [d-max (apply max ds)] ...)
               (for*/fold ([grid* grid])
                          ([d (in-range+ d-min d-max)] ...)
                 (let* ([cube (hash-ref grid (list d ...) #\.)]
                        [neighbours
                         (for*/list ([d* (in-range+ d d)] ...
                                     #:unless (and (= d d*) ...))
                           (hash-ref grid (list d* ...) #\.))]
                        [active-neighbours (actives neighbours)])
                   (cond
                     [(and (char=? cube #\#) (not (<= 2 active-neighbours 3)))
                      (hash-set grid* (list d ...) #\.)]
                     [(and (char=? cube #\.) (= active-neighbours 3))
                      (hash-set grid* (list d ...) #\#)]
                     [else grid*]))))))])))

(define part1
  (actives (hash-values ((iterate #{cycle 3 %} 6) (grid 3)))))

(define part2
  (actives (hash-values ((iterate #{cycle 4 %} 6) (grid 4)))))

(show-solution part1 part2)
