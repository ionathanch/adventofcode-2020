#lang curly-fn racket

(require "../lib.rkt")

(define input (lists->vectors (map string->list (problem-input 11))))
(define width (vector-length (vector-ref input 0)))
(define length (vector-length input))

(define (neighbours seats r c)
  (count
   #{char=? % #\#}
   (for*/list ([r* (range (max 0 (sub1 r)) (min (+ r 2) length))]
               [c* (range (max 0 (sub1 c)) (min (+ c 2) width))]
               #:when (not (and (= r r*) (= c c*))))
     (vector-ref (vector-ref seats r*) c*))))

(define (visible-in seats r c dr dc)
  (define rs
    (cond
      [(positive? dr) (range r length dr)]
      [(negative? dr) (range r -1 dr)]
      [else (build-list length (const r))]))
  (define cs
    (cond
      [(positive? dc) (range c width dc)]
      [(negative? dc) (range c -1 dc)]
      [else (build-list width (const c))]))
  (for/or ([r* rs]
           [c* cs]
           #:when (not (and (= r r*) (= c c*))))
    (define seat (vector-ref (vector-ref seats r*) c*))
    #:final (member seat '(#\L #\#))
    (char=? seat #\#)))

(define (visible seats r c)
  (count identity (map (match-lambda [`(,dr ,dc) (visible-in seats r c dr dc)])
                       '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1)))))

(define (step-seats counter die seats)
  (let ([new-seats (vector-map vector-copy (vector-copy seats))])
    (for* ([r (range 0 length)]
           [c (range 0 width)])
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
  (let loop ([seats input] [steps 0])
    (let ([new-seats (step-seats counter die seats)])
      (if (equal? seats new-seats)
          (vector-count #{char=? % #\#}
                        (apply vector-append (vector->list seats)))
          (loop new-seats (add1 steps))))))

(define part1
  (stable-count neighbours 4))

(define part2
  (stable-count visible 5))

(show-solution part1 part2)
