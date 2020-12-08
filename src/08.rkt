#lang racket

(require racket/set
         "../lib.rkt")

(define (parse instr)
  (match instr
    [(regexp #rx"(.+) (.+)" (list _ op arg))
     (cons (string->symbol op) (string->number arg))]))

(define input (list->vector (map parse (problem-input 8))))

(define (run instrs [part1 #f])
  (let loop ([pc 0] [acc 0] [ran (set)])
    (cond
      [(>= pc (vector-length instrs)) acc]
      [(set-member? ran pc) (if part1 acc #f)]
      [else (match (vector-ref instrs pc)
              [`(nop . ,arg) (loop (add1 pc) acc (set-add ran pc))]
              [`(acc . ,arg) (loop (add1 pc) (+ acc arg) (set-add ran pc))]
              [`(jmp . ,arg) (loop (+ pc arg) acc (set-add ran pc))])])))

(define (try-run instrs)
  (for/last ([i (range (vector-length instrs))]
             #:when (member (car (vector-ref instrs i)) '(nop jmp)))
    (define acc
      (match-let* ([`(,op . ,arg) (vector-ref instrs i)]
                   [op (if (symbol=? op 'nop) 'jmp 'nop)])
        (or (run instrs)
            (run (vector-set!* instrs i `(,op . ,arg))))))
    #:final acc
    acc))

(define-values (part1 part2)
  (values (run input #t) (try-run input)))

(show-solution part1 part2)
