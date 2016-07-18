#lang racket/base
(require racket/cmdline)
(require racket/file)
(define (rall str hold)
  (if (string=? str "") hold
      (let ([fst (substring str 0 1)] [rst (substring str 1)])
        (if (string=? fst "\n")
            (rall rst (string-append hold "\r\n"))
            (rall rst (string-append hold fst))))))
(let* ([filen (command-line #:args (filename) filename)]
       [str (file->string filen)])
  (write-string (rall str "") (open-output-file filen #:exists 'replace)))
; rall.
; 2016.07.19
