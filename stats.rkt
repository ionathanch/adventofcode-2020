#lang curly-fn racket

(require threading
         (only-in 2htdp/batch-io
                  read-lines))

(define (source-input n)
  (let* ([filename (~a n #:min-width 2 #:align 'right #:left-pad-string "0")]
         [path     (string-append "src/" filename ".rkt")])
    (read-lines path)))

(define srcs
  (~>> (range 1 11)
       (map (λ~>> source-input
                  (filter non-empty-string?)))))

(define src-lengths (map length srcs))

(define src-widths
  (~>> srcs
       (apply append)
       (map string-length)
       (sort _ <=)))

(define src-widths-string
  (string-join (map number->string src-widths) ","))
