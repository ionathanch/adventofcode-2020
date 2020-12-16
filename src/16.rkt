#lang curly-fn racket

(require "../lib.rkt")

(define input (problem-input-grouped 16))

(define valid-range/cs
  (map (match-lambda
         [(regexp #px".+: (\\d+)-(\\d+) or (\\d+)-(\\d+)" (list _ from1 to1 from2 to2))
          (or/c (integer-in (string->number from1) (string->number to1))
                (integer-in (string->number from2) (string->number to2)))])
       (string-lines (first input))))

(define any-valid-ranges/c
  (apply or/c valid-range/cs))

(define ticket
  (string-csv (second (string-lines (second input))) string->number))

(define tickets
  (map #{string-csv % string->number}
       (rest (string-lines (third input)))))

(define part1
  (for*/sum ([ticket tickets]
             [field ticket]
             #:unless (any-valid-ranges/c field))
    field))

(define part2
  (let ([fields (length ticket)]
        [valid-tickets (filter #{andmap any-valid-ranges/c %} tickets)])
    (define valid-ranges-fields
      (for*/fold ([valid-ranges-fields (make-list fields '())])
                 ([i fields]
                  [j fields])
        (if (andmap #{(list-ref valid-range/cs j) (list-ref % i)} valid-tickets)
            (list-update valid-ranges-fields j #{cons i %})
            valid-ranges-fields)))
    (define (solve valid-ranges-fields)
      (let ([j (index-where valid-ranges-fields (and/c list? singleton?))])
        (if j
            (let* ([i (first (list-ref valid-ranges-fields j))])
              (solve (map #{if (list? %) (remove i %) %}
                          (list-set valid-ranges-fields j i))))
            valid-ranges-fields)))
    (apply * (map #{list-ref ticket %}
                  (take (solve valid-ranges-fields) 6)))))

(show-solution part1 part2)
