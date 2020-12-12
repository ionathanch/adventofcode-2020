#lang curly-fn racket

(require threading
         "../lib.rkt")

(define input (lists->vectors (map string->list (problem-input 11))))
(define width (vector-length (vector-ref input 0)))
(define length (vector-length input))

(define first-seats
  (for*/hash ([r (in-range 0 length)]
              [c (in-range 0 width)])
    (values (list r c)
            (for*/list ([dr '(-1 0 1)]
                        [dc '(-1 0 1)]
                        #:unless (and (zero? dr) (zero? dc)))
              (let loop ([r (+ r dr)]
                         [c (+ c dc)])
                (cond
                  [(not (and (<= 0 r (sub1 length))
                             (<= 0 c (sub1 width)))) #f]
                  [(char=? (vector-ref (vector-ref input r) c) #\L) (list r c)]
                  [else (loop (+ r dr) (+ c dc))]))))))

(define (neighbours seats r c)
  (for*/sum ([r* (in-range (max 0 (sub1 r)) (min (+ r 2) length))]
             [c* (in-range (max 0 (sub1 c)) (min (+ c 2) width))]
             #:unless (and (= r r*) (= c c*)))
    (if (char=? (vector-ref (vector-ref seats r*) c*) #\#) 1 0)))

(define (visible seats r c)
  (count
   (match-lambda [`(,r* ,c*) (char=? (vector-ref (vector-ref seats r*) c*) #\#)] [#f #f])
   (hash-ref first-seats (list r c))))

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
