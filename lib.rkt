
#lang racket

(require
  (only-in data/queue
           make-queue
           enqueue!)
  (only-in 2htdp/batch-io
           read-lines
           read-file))

(provide problem-input
         problem-input-all
         problem-input-grouped
         show-solution

         make-vector-grid
         vector-grid-update
         lists->vectors
         vectors->lists
         hash->vectors
         show-list-grid
         show-vector-grid
         show-hash-grid

         ∘ ∂ ∂r $ %
         uncurry
         apply*
         apply-when

         string->number*
         string->symbol*

         nchar=?
         char-alphanumeric?

         sum
         !=
         nzero?
         negate
         pos-or-zero
         number->digits
         number->digits-reverse
         digits->number

         rac
         scanl scanr
         list-ref*
         repeat
         chunks-of
         transpose
         list->queue

         vector-first
         vector-last
         vector-ref*
         vector-grid-ref*
         vector-set!*
         hash->vector
         vector->hash)


;; Function helpers ;;
(define ∘ compose)
(define ∂ curry)
(define ∂r curryr)

;; uncurry : (a1 -> ... -> an -> b) -> ((listof a) -> b)
(define uncurry
  (curry apply))
(define $ uncurry)

;; apply* : (a -> b) -> a -> b
(define (apply* f a)
  (f a))

;; apply-when : a -> (a -> b) -> b
;; Apply given function only when a is not #f; return #f otherwise
(define (apply-when p f)
  (and p (f p)))


;; IO helpers ;;

;; problem-input : number? -> (listof string?)
;; Return contents of input file input/xx.txt as lines of strings.
(define (problem-input n)
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (string-append "../input/" filename ".txt")])
    (read-lines path)))

;; problem-input-all : number? -> string?
;; Return contents of input file input/xx.txt as a single string.
(define (problem-input-all n)
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (string-append "../input/" filename ".txt")])
    (read-file path)))

;; problem-input-grouped : number? -> (listof string?)
;; Return contents of input file input/xx.txt as a list of strings,
;; where each string is a group of lines separated by newlines.
(define (problem-input-grouped n)
  (string-split (problem-input-all n) "\n\n"))

;; show-solution : a -> b -> void
;; Print part1 and part2 on separate lines.
(define (show-solution part1 part2)
  (printf "Part 1: ~a\nPart 2: ~a\n" part1 part2))


;; Grid helpers ;;
;; A grid of values might be stored in three different ways:
;; - As a hashtable from positions (number . number) to values; or
;; - As a vector of vectors of values; or
;; - As a list of lists of values.

;; make-vector-grid : number -> number -> number -> vector-grid
(define (make-vector-grid width height [default 0])
  (build-vector height (λ (_) (make-vector width default))))

;; vector-grid-update : vector-grid -> (number . number) -> a -> void
;; Set the vector grid to given value at position (row, col)
(define (vector-grid-update vector-grid pos value)
  (vector-set! (vector-ref vector-grid (car pos)) (cdr pos) value))

;; lists->vectors : list-grid -> vector-grid
(define (lists->vectors list-grid)
  (list->vector (map list->vector list-grid)))

;; vectors->lists : vector-grid -> list-grid
(define (vectors->lists vector-grid)
  (map vector->list (vector->list vector-grid)))

;; hash->vectors : hash-grid -> number -> vector-grid
;; Where the position is not in the hash-grid,
;; the vector-grid takes on the default value.
(define (hash->vectors hash-grid [default 0])
  (let* ([keys (hash-keys hash-grid)]
         [xs (map car keys)]
         [ys (map cdr keys)]
         [min-x (apply min xs)]
         [min-y (apply min ys)]
         [width  (add1 (- (apply max xs) min-x))]
         [height (add1 (- (apply max ys) min-y))]
         [vector-grid (make-vector-grid width height default)])
    (hash-for-each
     hash-grid (λ (pos val)
                 (let ([x (- (car pos) min-x)]
                       [y (- (cdr pos) min-y)])
                   (vector-set! (vector-ref vector-grid y) x val))))
    vector-grid))

;; show-list-grid : (hashof (value => char)) -> list-grid -> void
(define (show-list-grid char-hash list-grid)
  (for-each
   displayln
   (map (∘ list->string
           (∂ map (∂ hash-ref char-hash)))
        list-grid)))

;; show-vector-grid : (hashof (value => char)) -> vector-grid -> void
(define (show-vector-grid char-hash vector-grid)
  (show-list-grid char-hash (vectors->lists vector-grid)))

;; show-hash-grid : (hashof (value => char)) -> hash-grid -> number -> void
(define (show-hash-grid char-hash hash-grid [default 0])
  (show-vector-grid char-hash (hash->vectors hash-grid default)))


;; Conversion helpers ;;

;; string->number* : (or/c string? #f) -> (or/c number? #f)
(define (string->number* s)
  (and (string? s) (string->number s)))

;; string->symbol* : (or/c string? #f) -> (or/c symbol? #f)
(define (string->symbol* s)
  (and (string? s) (string->symbol s)))


;; Char helpers ;;

;; nchar=? : char -> char -> boolean
(define (nchar=? c1 c2)
  (not (char=? c1 c2)))

;; char-alphanumeric? : char -> boolean
(define (char-alphanumeric? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))


;; Number helpers ;;

;; sum : (listof number) -> number
(define (sum ns) (apply + ns))

;; != : number -> number -> boolean
(define (!= n1 n2)
  (not (= n1 n2)))

;; nzero? : number -> boolean
(define (nzero? n)
  (not (zero? n)))

;; negate : number -> number
(define (negate n)
  (- 0 n))

;; pos-or-zero : number -> number
(define (pos-or-zero n)
  (if (negative? n) 0 n))

;; % : number -> number -> number
(define % modulo)

;; number->digits-reverse : number -> (listof number)
;; Return the digits of the given number in reverse order (i.e. RTL)
(define (number->digits-reverse n)
  (if (< n 10)
      (list n)
      (cons (remainder n 10)
            (number->digits-reverse (quotient n 10)))))

;; number->digits : number -> (listof number)
;; Return the digits of the given number (LTR)
(define (number->digits n)
  (reverse (number->digits-reverse n)))

;; digits->number : (listof number) -> number
;; Return the given digits as a number
(define (digits->number ns)
  (let loop ([n 0] [ns ns])
    (if (empty? ns) n
        (loop (+ (* n 10) (car ns)) (cdr ns)))))


;; List helpers ;;

;; rac : (listof any) -> any -> (listof any)
;; Append element to the back of the list.
(define (rac lst v)
  (append lst (list v)))

;; scanl : (a -> a -> a) -> (listof a) -> (listof a)
;; foldl that accumulates partial results in a list
(define (scanl f init lst)
  (reverse
   (foldl (λ (v lst)
            (cons (f v (first lst)) lst))
          (list init) lst)))

;; scanr : (a -> a -> a) -> (listof a) -> (listof a)
;; foldr that accumulates partial results in a list
(define (scanr f init lst)
  (reverse
   (foldr (λ (v lst)
            (cons (f v (first lst)) lst))
          (list init) lst)))

;; list-ref* : (listof a) -> number -> a -> a
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (list-ref* lst pos failure-result)
  (if (>= pos (length lst))
      failure-result
      (list-ref lst pos)))

;; repeat : number -> (listof any) -> (listof any)
(define (repeat m lst)
  (if (zero? m) '()
      (append lst (repeat (sub1 m) lst))))

;; chunks-of : (listof any) -> nonzero? -> (listof (listof any))
;; Partitions a list into lists of the given size in order,
;; with the final list possibly being smaller
;; e.g. '(1 2 3 4 5) 2 => '((1 2) (3 4) (5))
(define (chunks-of lst size)
  (if (< (length lst) size) lst
      (cons (take lst size)
            (chunks-of (drop lst size) size))))

;; transpose : (listof (listof any)) -> (listof (listof any))
;; Turns a list of lists into a list of lists of
;; the first elements of the lists, ..., the nth elements
;; where n is the length of the shortest list.
;; In short, it transposes a list of rows into a list of columns.
;; e.g. '((1 2 3 4)          '((1 5 8)
;;        (5 6 7)         =>   (2 6 9)
;;        (8 9 10 11 12))      (3 7 10))
(define (transpose lists)
  (let* ([min-len (apply min (map length lists))]
         [lists (map (λ (lst) (take lst min-len)) lists)])
    (apply map list lists)))

;; list->queue : (listof a) -> (queueof a)
;; Creates a queue and adds elements of list in order
(define (list->queue lst)
  (let ([Q (make-queue)])
    (for-each (∂ enqueue! Q) lst)
    Q))


;; Vector helpers ;;

;; vector-first : (vectorof any) -> any
(define (vector-first vec)
  (vector-ref vec 0))

;; vector-last : (vectorof any) -> any
(define (vector-last vec)
  (vector-ref vec (sub1 (vector-length vec))))

;; vector-ref* : (vectorof any) -> number -> any -> any
;; Same as list-ref, except a default value is provided
;; if the index is beyond the length of the list.
(define (vector-ref* vec pos failure-result)
  (if (>= pos (vector-length vec))
      failure-result
      (vector-ref vec pos)))

;; vector-grid-ref* : (vectorof (vectorof any)) -> (list number number) -> any -> any
;; Given coordinates (x, y), in the yth vector, find the xth element.
;; If either x or y are beyond the indices of the vectors,
;; return the default value provided.
(define (vector-grid-ref* grid coord failure-result)
  (match-let ([(list x y) coord]
              [y-len (vector-length grid)])
    (if (or (< y 0) (>= y y-len))
        failure-result
        (let* ([row (vector-ref grid y)]
               [x-len (vector-length row)])
          (if (or (< x 0) (>= x x-len))
              failure-result
              (vector-ref row x))))))

;; vector-set!* : (vectorof any) -> number -> any -> (vectorof any)
;; Set the value at given index in a new vector, then return that vector
;; If the index is beyond the indices of the vector,
;; a vector that can accomodate that index is returned,
;; with all the original elements and the element at the index set
(define (vector-set!* vec pos v)
  (let ([new-vec (make-vector (max (vector-length vec) (add1 pos)))])
    (vector-copy! new-vec 0 vec)
    (vector-set! new-vec pos v)
    new-vec))

;; hash->vector : (hashof (number => a)) -> (vectorof a)
;; Convert an intmap into a mutable vector
(define (hash->vector hash [default 0])
  (let ([length (add1 (apply max (hash-keys hash)))])
    (build-vector length (λ (i) (hash-ref hash i default)))))

;; vector->hash : (vectorof a) -> (hashof (number => a))
;; Convert a vector into an immutable intmap
(define (vector->hash vec)
  (let ([kvs (map cons (range (vector-length vec)) (vector->list vec))])
    (make-immutable-hash kvs)))