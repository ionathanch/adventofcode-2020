#lang racket

(require "../lib.rkt")

(define test
  (lists->vectors
   (map string->list
        '("..##......."
          "#...#...#.."
          ".#....#..#."
          "..#.#...#.#"
          ".#...##..#."
          "..#.##....."
          ".#.#.#....#"
          ".#........#"
          "#.##...#..."
          "#...##....#"
          ".#..#...#.#"))))

(define input (lists->vectors (map string->list (problem-input 3))))

(define (trees grid right down)
  (define width (vector-length (vector-ref grid 0)))
  (for/sum ([i (range 0 (vector-length grid) down)])
    (if (char=? #\#
                (vector-ref (vector-ref grid i)
                            (% (* (/ i down) right) width)))
        1 0)))

(define (test-input grid)
  (define (print-test right down)
    (let ([trees (trees grid right down)])
      (printf "Part 2 test (~a right, ~a down): ~a\n"
              right down trees)
      trees))
  (printf "Part 2 test: ~a\n"
          (* (print-test 1 1)
             (print-test 3 1)
             (print-test 5 1)
             (print-test 7 1)
             (print-test 1 2))))

(define part1
  (trees input 3 1))

(define part2
  (* (trees input 1 1)
     (trees input 3 1)
     (trees input 5 1)
     (trees input 7 1)
     (trees input 1 2)))

(test-input test)
(show-solution part1 part2)
