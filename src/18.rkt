#lang curly-fn racket

(require "../lib.rkt")

(define input
  (map #{read (open-input-string (format "(~a)" %))} (problem-input 18)))

(define (eval1 sexp)
  (match sexp
    [`,n #:when (number? n) n]
    [`(,tail) (eval1 tail)]
    [`(,rest ... + ,tail)
     (+ (eval1 tail) (eval1 rest))]
    [`(,rest ... * ,tail)
     (* (eval1 tail) (eval1 rest))]))

(define (eval2 sexp)
  (match sexp
    [`,n #:when (number? n) n]
    [`(,tail) (eval2 tail)]
    [`(,head ... ,left + ,right ,tail ...)
     (eval2 `(,@head ,(+ (eval2 left) (eval2 right)) ,@tail))]
    [`(,head ... ,left * ,right ,tail ...)
     (eval2 `(,@head ,(* (eval2 left) (eval2 right)) ,@tail))]))

(define part1
  (sum (map eval1 input)))

(define part2
  (sum (map eval2 input)))

(show-solution part1 part2)
