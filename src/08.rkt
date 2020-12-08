#lang racket

(require racket/set
         "../lib.rkt")

(define (parse instr)
  (match instr
    [(regexp #rx"(.+) (.+)" (list _ op arg))
     (cons (string->symbol op) (string->number arg))]))

(define input (list->vector (map parse (problem-input 8))))

(define (run instrs try)
  (let loop ([pc 0] [acc 0] [ran (set)] [backtrack (const #f)])
    (define (get-backtrack next-pc)
      (if (and try (= pc try))
          (const (loop next-pc acc (set-add ran pc) (const #f)))
          backtrack))
    (cond
      [(>= pc (vector-length instrs)) acc]
      [(set-member? ran pc) (if try (backtrack) acc)]
      [else (match (vector-ref instrs pc)
              [`(acc . ,arg) (loop (add1 pc) (+ acc arg) (set-add ran pc) backtrack)]
              [`(nop . ,arg) (loop (add1 pc) acc (set-add ran pc) (get-backtrack (+ pc arg)))]
              [`(jmp . ,arg) (loop (+ pc arg) acc (set-add ran pc) (get-backtrack (add1 pc)))])])))

(define (try-run instrs)
  (for/or ([i (range (vector-length instrs))]
           #:when (member (car (vector-ref instrs i)) '(nop jmp)))
    (match-let* ([`(,op . ,arg) (vector-ref instrs i)])
      (run instrs i))))

(define-values (part1 part2)
  (values (run input #f) (try-run input)))

(show-solution part1 part2)
