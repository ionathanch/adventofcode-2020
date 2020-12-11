#lang curly-fn racket

(require "../lib.rkt")

(define input (lists->vectors (map string->list (problem-input 11))))
(define width (vector-length (vector-ref input 0)))
(define length (vector-length input))

(define (neighbours seats r c)
  (count
   #{char=? % #\#}
   (for*/list ([r* (in-range (max 0 (sub1 r)) (min (+ r 2) length))]
               [c* (in-range (max 0 (sub1 c)) (min (+ c 2) width))]
               #:unless (and (= r r*) (= c c*)))
     (vector-ref (vector-ref seats r*) c*))))

(define (visible-in seats r c dr dc)
  (define rs
    (cond
      [(positive? dr) (in-range r length dr)]
      [(negative? dr) (in-range r -1 dr)]
      [else (in-cycle `(,r))]))
  (define cs
    (cond
      [(positive? dc) (in-range c width dc)]
      [(negative? dc) (in-range c -1 dc)]
      [else (in-cycle `(,c))]))
  (for/or ([r* rs]
           [c* cs]
           #:unless (and (= r r*) (= c c*)))
    (define seat (vector-ref (vector-ref seats r*) c*))
    #:final (member seat '(#\L #\#))
    (char=? seat #\#)))

(define (visible seats r c)
  (count (match-lambda [`(,dr ,dc) (visible-in seats r c dr dc)])
         '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))))

(define (step-seats counter die seats)
  (let ([new-seats (vector-map vector-copy (vector-copy seats))])
    (for* ([r (in-range 0 length)]
           [c (in-range 0 width)])
      (let ([seat (vector-ref (vector-ref seats r) c)])
        (cond
          [(and (char=? seat #\L)
                (zero? (counter seats r c)))
           (vector-set! (vector-ref new-seats r) c #\#)]
          [(and (char=? seat #\#)
                (>= (counter seats r c) die))
           (vector-set! (vector-ref new-seats r) c #\L)])))
    new-seats))

(define (stable-count counter die)
  (let loop ([seats input])
    (let ([new-seats (step-seats counter die seats)])
      (if (equal? seats new-seats)
          (vector-count #{char=? % #\#}
                        (apply vector-append (vector->list seats)))
          (loop new-seats)))))

(define part1
  (stable-count neighbours 4))

(define part2
  (stable-count visible 5))

(show-solution part1 part2)
