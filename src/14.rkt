#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input 14))

(define (parse1 instr)
  (match instr
    [(regexp #px"^mask = (\\w+)$" (list _ mask))
     (let ([ormask (string->binary (string-replace mask "X" "0"))]
           [andmask (string->binary (string-replace mask "X" "1"))])
       (list 'mask ormask andmask))]
    [(regexp #px"^mem\\[(\\d+)\\] = (\\d+)" (list _ addr val))
     (list 'write (string->number addr) (string->number val))]))

(define (parse2 instr)
  (match instr
    [(regexp #px"^mask = (\\w+)$" (list _ mask))
     (let* ([ormask (string->binary (string-replace mask "X" "0"))]
            [andmask (string->binary (string-replaces mask '(["0" "1"] ["X" "0"])))]
            [X-indices (indexes-of (string->list mask) #\X)]
            [X-ormasks (map #{expt 2 (- (sub1 (string-length mask)) %)} X-indices)])
       (list 'mask ormask andmask X-ormasks))]
    [(regexp #px"^mem\\[(\\d+)\\] = (\\d+)" (list _ addr val))
     (list 'write (string->number addr) (string->number val))]))

(define (addrs addr ormasks)
  (if (empty? ormasks)
      (list addr)
      (let ([rest-addrs (addrs addr (rest ormasks))])
        (append (map #{bitwise-ior (first ormasks) %} rest-addrs)
                rest-addrs))))

(define part1
  (for/fold ([mem (hash)]
             [ormask 0]
             [andmask -1]
             #:result (sum (hash-values mem)))
            ([instr (sequence-map parse1 input)])
    (match instr
      [(list 'mask ormask andmask)
       (values mem ormask andmask)]
      [(list 'write addr val)
       (let ([val (bitwise-ior ormask (bitwise-and andmask val))])
         (values (hash-set mem addr val) ormask andmask))])))

(define part2
  (for/fold ([mem (hash)]
             [ormask 0]
             [andmask -1]
             [X-ormasks '()]
             #:result (sum (hash-values mem)))
            ([instr (sequence-map parse2 input)])
    (match instr
      [(list 'mask ormask andmask X-ormasks)
       (values mem ormask andmask X-ormasks)]
      [(list 'write addr val)
       (let* ([addr (bitwise-ior ormask (bitwise-and andmask addr))]
              [mem (foldl #{hash-set %2 %1 val} mem (addrs addr X-ormasks))])
         (values mem ormask andmask X-ormasks))])))

(show-solution part1 part2)
