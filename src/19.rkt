#lang curly-fn racket

(require "../lib.rkt")

(define (parse-rule rule)
  (match rule
    [(regexp #px"(\\d+): (.+)" (list _ rule alts))
     (cons (string->number rule)
           (~>> alts
                (string-replace _ "|" ")(")
                (format "((~a))")
                open-input-string
                read))]))

(define-values (rules msgs)
  (match-let ([(list rules msgs) (problem-input-grouped 19)])
    (values (make-hash (map parse-rule (string-lines rules)))
            (string-lines msgs))))

(define (ruleset start)
  (list->set
   ;; loop: number? -> (listof string?)
   (let loop ([rule start])
     (apply append
            (map #{match %
                    [`(,l) #:when (string? l) %]
                    [`(,rs ...) (map #{apply string-append %}
                                     (apply cartesian-product
                                            (map #{loop %} rs)))]}
                 (hash-ref rules rule))))))

(define rule42 (ruleset 42))
(define rule31 (ruleset 31))
(define len (string-length (set-first rule42)))

(define (string-group len str)
  (for/list ([group (/ (string-length str) len)])
    (substring str (* group len) (* (add1 group) len))))

(define (matches-rules-1 str)
  (and (= (string-length str) (* 3 len))
       (let ([strs (string-group len str)])
         (and (set-member? rule42 (first strs))
              (set-member? rule42 (second strs))
              (set-member? rule31 (third strs))))))

(define (matches-rules-2 str)
  (and (zero? (remainder (string-length str) len))
       (let* ([strs (string-group len str)]
              [count (length strs)])
         (for/or ([c (in-range (add1 (floor (/ count 2))) count)])
           (and (andmap #{set-member? rule42 %} (take strs c))
                (andmap #{set-member? rule31 %} (drop strs c)))))))

(define-values (part1 part2)
  (values (count #{matches-rules-1 %} msgs)
          (count #{matches-rules-2 %} msgs)))

(show-solution part1 part2)
