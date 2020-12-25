#lang racket

(require math/number-theory
         "../lib.rkt")

(define card 9033205)
(define door 9281649)

(define door-loop
  (for/first ([loop (in-naturals)]
              #:when (= (modular-expt 7 loop 20201227) door))
    loop))

(define part1
  (modular-expt card door-loop 20201227))

(show-solution part1 "Done!")
