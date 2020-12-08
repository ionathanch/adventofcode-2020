#lang curly-fn racket

(require racket/set
         (except-in graph transpose)
         "../lib.rkt")

;; op? = (one-of/c 'acc 'nop 'jmp)
;; instr? = (cons/c op? number?)
;; parse: string? -> instr?
(define (parse instr)
  (match instr
    [(regexp #rx"(.+) (.+)" (list _ op arg))
     (cons (string->symbol op) (string->number arg))]))

;; absolutize: instr? -> instr?
;; Turns relative jumps into absolute jumps,
;; treating nop arguments the same
(define (absolutize instrs)
  (map (λ (instr i)
         (match instr
           [`(acc . ,arg) instr]
           [`(,op . ,arg) `(,op . ,(+ arg i))]))
       instrs (range (length instrs))))

;; instrs? = (lisof instr?)
;; input: instrs?
(define input (list->vector (absolutize (map parse (problem-input 8)))))

;; run: instrs? -> number?
;; Runs a sequence of instructions with absolute jumps
(define (run instrs)
  (let loop ([pc 0] [acc 0] [ran (set)])
    (cond
      [(>= pc (vector-length instrs)) acc]
      [(set-member? ran pc) acc]
      [else (match (vector-ref instrs pc)
              [`(acc . ,arg) (loop (add1 pc) (+ acc arg) (set-add ran pc))]
              [`(nop . ,arg) (loop (add1 pc) acc (set-add ran pc))]
              [`(jmp . ,arg) (loop arg acc (set-add ran pc))])])))

;; graph? = (unweighted-undirected-graphof number? number?)
;; graph-to? = (unweighted-directed-graphof number? number?)
;; call-graphs: instrs? -> graph? graph-to?
;; Call graphs for the input sequence of instructions,
;; where nodes are the instruction indices or 'END
(define (call-graphs instrs)
  (let ([graph (unweighted-graph/undirected '())]
        [graph-to (unweighted-graph/directed '())]
        [len (vector-length instrs)])
    (define (add-edge-to! from to)
      (add-edge! graph from to)
      (add-directed-edge! graph-to from to))
    (for ([instr instrs]
          [i (range len)])
      (match instr
        [`(jmp . ,arg) #:when (>= arg len) (add-edge-to! i 'END)]
        [_ #:when (>= (add1 i) len) (add-edge-to! i 'END)]
        [`(jmp . ,arg) (add-edge-to! i arg)]
        [else (add-edge-to! i (add1 i))]))
    (values graph graph-to)))

#|
The key observation here is that there are only two connected components in the call graph:
one that includes the starting instruction (index 0), and
one that includes the final instruction (jumping beyond the last index).
We then only need to search through all of the jump and nop instructions that can be reached,
and find the one such that when swapped to a nop or jump instruction,
will land execution into the component containing the final instruction.
|#

;; start-jmpnops: graph-to? -> (setof number?)
;; A set of jump and nop instruction indices reachable from the first instruction
(define (start-jmpnops graph-to)
  (define-values (start-dijkstra _)
    (dijkstra graph-to 0))
  (~>> start-dijkstra
       hash->list
       (filter (λ~> cdr (!= +inf.0)))
       (map car)
       (filter #{member (car (vector-ref input %)) '(jmp nop)})
       list->set))

;; end-instrs: graph? -> (setof number?)
;; A set of instruction indices that can reach the end of execution
(define (end-instrs graph)
  (~>> (cc graph)
       (filter #{member 'END %})
       first
       (filter number?)
       list->set))

;; bridge: instrs? -> number?
;; The instruction index such that,
;; when swapped from a jump to a nop or vice versa,
;; will allow execution from the start to be able to reach the end,
;; bridging the gap between the start and end connected components
(define (bridge instrs)
  (let*-values ([(graph graph-to) (call-graphs instrs)]
                [(starts) (start-jmpnops graph-to)]
                [(ends) (end-instrs graph)])
    (for/or ([i (in-set starts)])
      (and (match (vector-ref input i)
             [`(jmp . ,arg) (set-member? ends (add1 i))]
             [`(nop . ,arg) (set-member? ends arg)])
           i))))

;; swap-instr: instrs? number? -> instrs?
;; Swap the instruction at the given index
;; from a jump to a nop or vice versa
(define (swap-instr instrs i)
  (match (vector-ref instrs i)
    [`(jmp . ,arg) (vector-set!* instrs i `(nop . ,arg))]
    [`(nop . ,arg) (vector-set!* instrs i `(jmp . ,arg))]
    [instr instrs]))

(define-values (part1 part2)
  (values (run input) (run (swap-instr input (bridge input)))))

(show-solution part1 part2)
