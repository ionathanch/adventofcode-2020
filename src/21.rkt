#lang curly-fn racket

(require "../lib.rkt")

;; allergen = string
;; ingredient = string

;; parse: string? -> (listof (listof allergen?) (listof ingredient?))
(define (parse food)
  (match food
    [(regexp #px"(.+) \\(contains (.+)\\)" (list _ ingredients allergens))
     (list (string-split allergens ", ") (string-split ingredients " "))]))

;; input: (listof (listof (listof allergen?) (listof ingredient?)))
(define input (map parse (problem-input 21)))

;; foods: (hashof allergen? (listof (setof ingredient?)))
(define foods
  (for*/fold ([foods (hash)])
             ([food input]
              [allergen (first food)])
    (hash-update foods allergen #{cons (list->set (second food)) %} '())))

;; allergens: (listof allergen?)
(define allergens
  (hash-keys foods))

;; If the given allergen only has one possible allergenic,
;; remove that allergenic from the possible allergenics of every allergen
;; remove-allergenic: (hashof allergen? (setof ingredient?)) allergen? -> (hashof allergen? (setof ingredient?))
(define (remove-allergenic allergenics allergen)
  (let* ([ingredients (hash-ref allergenics allergen)]
         [ingredient (set-first ingredients)])
    (if (= (set-count ingredients) 1)
        (for/fold ([allergenics* allergenics])
                  ([allergen* allergens]
                   #:unless (string=? allergen allergen*))
          (hash-update allergenics* allergen* #{set-remove % ingredient}))
        allergenics)))

;; possible-allergenics: (hashof allergen? (setof ingredient?))
(define possible-allergenics
  (for/hash ([allergen allergens])
    (values allergen (apply set-intersect (hash-ref foods allergen)))))

;; allergenics: (listof (list allergen? ingredient?))
(define allergenics
  (let loop ([allergenics possible-allergenics])
    (for/fold ([allergenics* allergenics]
               #:result (if (equal? allergenics allergenics*)
                            (map #{list (car %) (set-first (cdr %))}
                                 (hash->list allergenics*))
                            (loop allergenics*)))
              ([allergen allergens])
      (remove-allergenic allergenics* allergen))))

(define part1
  (length (remove* (map second allergenics)
                   (apply append (map second input)))))

(define part2
  (~> allergenics
      (sort (λ (a1 a2) (string<=? (first a1) (first a2))))
      (map second _)
      (string-join ",")))

(show-solution part1 part2)
