#lang racket

(require "../lib.rkt")

(define input (problem-input-grouped 4))

(define test
  "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

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

(define (passports in)
  (let* ([passport-entries (map (compose
                                 (∂ map (compose
                                         (∂ map ∂ (list string->symbol identity))
                                         (∂r string-split ":")))
                                 (∂r string-split " ")
                                 (∂r string-replace "\n" " "))
                                in)]
         [passports (map (∂ foldl passport-set default-passport)
                         passport-entries)]
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

(define-values (part1 part2)
  (passports input))

(show-solution part1 part2)
