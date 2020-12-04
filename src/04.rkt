#lang racket

(require "../lib.rkt")

(define input (problem-input-all 4))

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

(define fields '(byr iyr eyr hgt hcl ecl pid)) ;; except 'cid
(define ecls '(amb blu brn gry grn hzl oth))

(define (passports in)
  (define (interp-value field value-string)
    (match field
      ['byr (string->number value-string)]
      ['iyr (string->number value-string)]
      ['eyr (string->number value-string)]
      ['hgt (cond
              [(string-suffix? value-string "in")
               (cons 'in (string->number (string-trim value-string "in")))]
              [(string-suffix? value-string "cm")
               (cons 'cm (string->number (string-trim value-string "cm")))]
              [else (cons 'na (string->number value-string))])]
      ['hcl (string->list value-string)]
      ['ecl (string->symbol value-string)]
      ['pid (string->list value-string)]
      ['cid value-string]))
  (let* ([passport-strings (map (λ (p) (string-replace p "\n" " "))
                                (string-split in "\n\n"))]
         [passport-entries (map (λ (p) (map (curryr string-split ":")
                                            (string-split p " ")))
                                passport-strings)]
         [passport-hashes (map (λ (p)
                                 (for/hash ([entry p])
                                   (let* ([field (string->symbol (first entry))]
                                          [value (interp-value field (second entry))])
                                     (values field value))))
                               passport-entries)])
    passport-hashes))

(define part1
  (count (λ (p) (andmap (∂ hash-has-key? p) fields))
         (passports input)))

(define part2
  (count
   (λ (p)
     (and (andmap (∂ hash-has-key? p) fields)
          (match-let ([byr (hash-ref p 'byr)]
                      [iyr (hash-ref p 'iyr)]
                      [eyr (hash-ref p 'eyr)]
                      [`(,unit . ,hgt) (hash-ref p 'hgt)]
                      [`(,hash ,hex ...) (hash-ref p 'hcl)]
                      [ecl (hash-ref p 'ecl)]
                      [pid (hash-ref p 'pid)])
            (and (<= 1920 byr 2002)
                 (<= 2010 iyr 2020)
                 (<= 2020 eyr 2030)
                 (match unit
                   ['cm (<= 150 hgt 193)]
                   ['in (<= 59 hgt 76)]
                   [else #f])
                 (char=? #\# hash)
                 (= (length hex) 6)
                 (andmap (curry char-alphanumeric?) hex)
                 (member ecl ecls)
                 (= (length pid) 9)))))
   (passports input)))

(show-solution part1 part2)
