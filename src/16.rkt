#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input-grouped 16))

(define valid-range/cs
  (map (match-lambda
         [(regexp #px".+: (\\d+)-(\\d+) or (\\d+)-(\\d+)" (list _ from1 to1 from2 to2))
          (or/c (integer-in (string->number from1) (string->number to1))
                (integer-in (string->number from2) (string->number to2)))])
       (string-split (first input) "\n")))

(define any-valid-ranges/c
  (apply or/c valid-range/cs))

(define ticket
  (map string->number (string-split (second (string-split (second input) "\n")) ",")))

(define tickets
  (map (λ~> (string-split ",") (map string->number _))
       (rest (string-split (third input) "\n"))))

(define part1
  (for*/sum ([ticket tickets]
             [field ticket])
    (if (any-valid-ranges/c field) 0 field)))

(define part2
  (let ([fields (length ticket)])
    (define valid-ranges-fields
      (for*/fold ([valid-ranges-fields (make-list fields (range fields))])
                 ([valid-ticket (filter #{andmap any-valid-ranges/c %} tickets)]
                  [i fields]
                  [j fields])
        (let ([valid-range/c (list-ref valid-range/cs j)]
              [valid-range-fields (list-ref valid-ranges-fields j)])
          (if (valid-range/c (list-ref valid-ticket i))
              valid-ranges-fields
              (list-set valid-ranges-fields j (remove i valid-range-fields))))))
    (define (solve valid-ranges-fields)
      (let ([j (index-where valid-ranges-fields (and/c list? singleton?))])
        (if j
            (let* ([i (first (list-ref valid-ranges-fields j))]
                   [valid-ranges-fields (map #{if (list? %) (remove i %) %}
                                             valid-ranges-fields)])
              (solve (list-set valid-ranges-fields j i)))
            valid-ranges-fields)))
    (apply * (map #{list-ref ticket %}
                  (take (solve valid-ranges-fields) 6)))))

(show-solution part1 part2)
