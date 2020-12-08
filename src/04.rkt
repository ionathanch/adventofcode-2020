#lang racket

(require "../lib.rkt")

(define input (problem-input-grouped 4))

(struct passport
  (byr iyr eyr hgt hcl ecl pid cid)
  #:transparent)

(define default-passport
  (passport #f #f #f #f #f #f #f #f))

(define (passport-set entry pass)
  (let ([value (second entry)])
    (match (first entry)
      ['byr (struct-copy passport pass [byr value])]
      ['iyr (struct-copy passport pass [iyr value])]
      ['eyr (struct-copy passport pass [eyr value])]
      ['hgt (struct-copy passport pass [hgt value])]
      ['hcl (struct-copy passport pass [hcl value])]
      ['ecl (struct-copy passport pass [ecl value])]
      ['pid (struct-copy passport pass [pid value])]
      ['cid (struct-copy passport pass [cid value])]
      [else pass])))

(define (hgt? str)
  (match str
    [(regexp #px"^(\\d{3})cm$" (list _ hgt)) (<= 150 (string->number hgt) 193)]
    [(regexp #px"^(\\d{2})in$" (list _ hgt)) (<= 59 (string->number hgt) 76)]
    [_ #f]))

(define-values (part1 part2)
  (let* ([passport-entries (map (λ~>> (string-split _ #px"[ \n]")
                                      (map (match-lambda [(regexp #rx"(.+):(.+)" (list _ field value))
                                                          (list (string->symbol field) value)])))
                                input)]
         [passports (map (∂ foldl passport-set default-passport) passport-entries)]
         [valid-raw? (struct/c passport string? string? string? string? string? string? string? any/c)]
         [valid? (struct/c passport
                           (and/c string? (∘ (integer-in 1920 2002) string->number))
                           (and/c string? (∘ (integer-in 2010 2020) string->number))
                           (and/c string? (∘ (integer-in 2020 2030) string->number))
                           hgt?
                           (and/c string? (∂ regexp-match-exact? #px"#[[:xdigit:]]{6}"))
                           (apply or/c '("amb" "blu" "brn" "gry" "grn" "hzl" "oth"))
                           (and/c string? (∂ regexp-match-exact? #px"\\d{9}"))
                           any/c)])
    (values (count valid-raw? passports)
            (count valid? passports))))

(show-solution part1 part2)
