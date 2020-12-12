#lang racket

(require math/matrix
         "../lib.rkt")

(define (parse line)
  (match line
    [(regexp #px"([NESWLRF])(\\d+)" (list _ dir amt))
     (list (string->symbol dir) (string->number amt))]))

(define input (map parse (problem-input 12)))

(define N (col-matrix [0 -1]))
(define E (col-matrix [1 0]))
(define S (col-matrix [0 1]))
(define W (col-matrix [-1 0]))
(define R90 (matrix [[0 -1] [1 0]]))
(define L90 (matrix [[0 1] [-1 0]]))

(define (move instr p1 p2 pos dir)
  (match instr
    [`(N ,amt) (values (matrix+ pos (matrix-scale N (* amt p1)))
                       (matrix+ dir (matrix-scale N (* amt p2))))]
    [`(E ,amt) (values (matrix+ pos (matrix-scale E (* amt p1)))
                       (matrix+ dir (matrix-scale E (* amt p2))))]
    [`(S ,amt) (values (matrix+ pos (matrix-scale S (* amt p1)))
                       (matrix+ dir (matrix-scale S (* amt p2))))]
    [`(W ,amt) (values (matrix+ pos (matrix-scale W (* amt p1)))
                       (matrix+ dir (matrix-scale W (* amt p2))))]
    [`(F ,amt) (values (matrix+ pos (matrix-scale dir amt)) dir)]
    [(or `(R 90) `(L 270)) (values pos (matrix* R90 dir))]
    [(or `(L 90) `(R 270)) (values pos (matrix* L90 dir))]
    [`(,_ 180) (values pos (matrix-scale dir -1))]))

(define part1
  (match-let-values
   ([(pos dir)
     (for/fold ([pos (col-matrix [0 0])]
                [dir E])
               ([instr input])
       (move instr 1 0 pos dir))])
   (matrix-1norm pos)))

(define part2
  (match-let-values
   ([(ship wpt)
     (for/fold ([ship (col-matrix [0 0])]
                [wpt (col-matrix [10 -1])])
               ([instr input])
       (move instr 0 1 ship wpt))])
   (matrix-1norm ship)))

(show-solution part1 part2)
