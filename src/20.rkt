#lang curly-fn racket

(require "../lib.rkt")

(define input (map string-lines (problem-input-grouped 20)))

;; name: number?
;; image: image? = (listof (listof char?))
(struct tile (name image) #:transparent)

(define (parse-tile tile-strings)
  (match-let ([`(,name ,rows ...) tile-strings])
    (tile (string->number (second (regexp-match #px"Tile (\\d+):" name)))
          (map string->list rows))))

(define tiles (map parse-tile input))
(define dim (sqrt (length tiles)))
(define size (length (tile-image (first tiles))))

;; Orientations (rotated and flipped)
(define (o1 image) image)
(define (o2 image) (reverse image))
(define (o3 image) (map reverse image))
(define (o4 image) (reverse (map reverse image)))
(define (o5 image) (map #{map (λ~> (list-ref %)) image} (range (length image))))
(define (o6 image) (map #{map (λ~> (list-ref %)) (reverse image)} (range (length image))))
(define (o7 image) (map #{map (λ~> (list-ref %)) image} (reverse (range (length image)))))
(define (o8 image) (map #{map (λ~> (list-ref %)) (reverse image)} (reverse (range (length image)))))

;; Image fits top and left when not false, respectively
;; fit-image: image? (or/c (listof char?) #f) (or/c (listof char?) #f) -> (or/c image? #f)
(define (fit-image image t l)
  (define (left image) (map first image))
  (or (let ([i (o1 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o2 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o3 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o4 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o5 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o6 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o7 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))
      (let ([i (o8 image)]) (and (or (not t) (equal? (first i) t)) (or (not l) (equal? (left i) l)) i))))

;; Tile's image fits top and left when not false, respectively
;; fit-tile: tile? (or/c (listof char?) #f) (or/c (listof char?) #f) -> (or/c tile? #f)
(define (fit-tile t top left)
  (let ([image (fit-image (tile-image t) top left)])
    (and image (struct-copy tile t [image image]))))

;; Given the 0th to (i-1)th tiles, counting left-to-right then top-to-bottom,
;; find the remaining ith to last tiles that fit in the dim x dim grid
;; build-image: (hashof (list number? number?) tile?) number? -> (hashof (list number? number?) tile?)
(define (build-image image tiles i)
  (let* ([r (quotient i dim)]
         [c (remainder i dim)]
         [top-tile (and (> r 0) (hash-ref image (list (sub1 r) c)))]
         [left-tile (and (> c 0) (hash-ref image (list r (sub1 c))))]
         [top (and top-tile (last (tile-image top-tile)))]
         [left (and left-tile (map last (tile-image left-tile)))])
    (for/or ([tile tiles])
      (let* ([tile* (fit-tile tile top left)]
             [image (hash-set image (list r c) tile*)])
        (and tile* (or (and (= (add1 i) (* dim dim)) image)
                       (build-image image (remove tile tiles) (add1 i))))))))

;; corner?: tile? -> boolean?
(define (corner? tile)
  (define (edges tile)
    (let ([image (tile-image tile)])
      (list (first image) (last image)
            (map first image) (map last image))))
  (let ([tile-edges (edges tile)])
    (= 2 (count #{for/or ([other (remove tile tiles)])
                   (let ([other-edges (edges other)])
                     (or (member % other-edges)
                         (member (reverse %) other-edges)))}
                tile-edges))))

;; Find a corner piece
;; corner: tile?
(define corner
  (for/first ([tile tiles]
              #:when (corner? tile))
    tile))

;; A mapping from positions in the layout to the correctly-oriented tile
;; (hashof (list number? number?) tile?)
(define image-hash
  (let ([corner (λ (o) (struct-copy tile corner [image (o (tile-image corner))]))]
        [tiles (remove corner tiles)])
    (or (build-image (hash (list 0 0) (corner o1)) tiles 1)
        (build-image (hash (list 0 0) (corner o2)) tiles 1)
        (build-image (hash (list 0 0) (corner o3)) tiles 1)
        (build-image (hash (list 0 0) (corner o4)) tiles 1))))

;; The full image with borders removed
;; image: (listof (listof char?))
(define image
  (for/fold ([image '()]
             #:result (o4 image))
            ([r dim])
    (append image
            (for/fold ([rows (make-list (- size 2) '())])
                      ([c dim])
              (let* ([image (tile-image (hash-ref image-hash (list r c)))]
                     [image (map #{drop-right (drop % 1) 1} (drop-right (drop image 1) 1))])
                (map append rows image))))))

(define dimsize (length image))
(define image-vectors (lists->vectors image))
(define (vector-pos r c) (vector-ref (vector-ref image-vectors r) c))

#| (r, c) = O
                  #
#    ##    ##    O##   <--- friend
 #  #  #  #  #  #
|#
(define (sea-monster-coords r c)
  (list `(,(- r 1) ,(+ c 1))
        `(,r ,c)
        `(,r ,(+ c 1))
        `(,r ,(+ c 2))
        `(,r ,(- c 5))
        `(,r ,(- c 6))
        `(,r ,(- c 11))
        `(,r ,(- c 12))
        `(,r ,(- c 17))
        `(,(+ r 1) ,(- c 1))
        `(,(+ r 1) ,(- c 4))
        `(,(+ r 1) ,(- c 7))
        `(,(+ r 1) ,(- c 10))
        `(,(+ r 1) ,(- c 13))
        `(,(+ r 1) ,(- c 16))))

;; sea-monster?: number? number? -> boolean?
(define (sea-monster? r c)
  (define (pound? c) (char=? c #\#))
  (andmap #{pound? (vector-pos (first %) (second %))}
          (sea-monster-coords r c)))

;; A list of the neck coordinates of sea monsters
;; sea-monsters: (listof (list number? number?))
(define sea-monsters
  (for*/list ([r (range 1 (sub1 dimsize))]
              [c (range 17 (- dimsize 2))]
              #:when (and (andmap #{char=? (vector-pos r %)}
                                  (range c (+ c 3)))
                          (sea-monster? r c)))
    (list r c)))

;; Replace the # of a sea monster with O in image-vectors
(define (reveal-sea-monster! r c)
  (for-each #{vector-set! (vector-ref image-vectors (first %)) (second %) #\O}
            (sea-monster-coords r c)))

;; Reveal the sea monsters in image-vectors, then write to file
(define (draw-sea-monsters!)
  (for-each #{reveal-sea-monster! (first %) (second %)} sea-monsters)
  (display-lines-to-file
   (map (λ~> vector->list list->string)
        (vector->list image-vectors))
   "../input/20-image.txt"
   #:exists 'replace))

(define part1
  (* (tile-name (hash-ref image-hash (list 0 0)))
     (tile-name (hash-ref image-hash (list 0 (sub1 dim))))
     (tile-name (hash-ref image-hash (list (sub1 dim) 0)))
     (tile-name (hash-ref image-hash (list (sub1 dim) (sub1 dim))))))

(define part2
  (- (count #{char=? % #\#} (apply append image))
     (* (length sea-monsters) 15)))

(show-solution part1 part2)
