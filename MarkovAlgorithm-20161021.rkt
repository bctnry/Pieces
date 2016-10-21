#lang racket
(require racket/control)
; markov algorithm.
; using spec from rosetta stone:
; http://rosettacode.org/wiki/Execute_a_Markov_algorithm
; bctnry, 20161021.

(define (mkRule from to terminate?)
  (cons (λ (t) (string-replace t from to #:all? #F)) terminate?))
(define (interpret ruleList)
  (define (interpret_ ruleList str original)
    (if (null? ruleList) (shift k (k str))
        (let* ([rule (caar ruleList)]
               [isTerminating? (cdar ruleList)]
               [isInterpreting? (not (string=? (rule str) str))])
          (if isInterpreting?
              (if isTerminating? (shift k (k (rule str)))
                  (interpret_ original (rule str) original))
              (interpret_ (cdr ruleList) str original)))))
  (λ (str) (reset (interpret_ ruleList str ruleList))))

(define (retrRule str)
  (let ([matchRes (regexp-match #rx"^(.*?)[ \t]*->[ \t]+(.*?)[ \t]*$" str)])
    (if matchRes
        (if (char=? #\. (string-ref (third matchRes) 0))
            (mkRule (second matchRes)
                    (substring (third matchRes) 1) #T)
            (mkRule (second matchRes) (third matchRes) #F))
        matchRes)))
(define (elimComment str)
  (regexp-match #rx".*?[ \t]*#[ \t]*.*" str))
(define (whiteLine? str) (regexp-match #rx"^[ \t]*$" str))
(define (denoteRules str)
  (let ([splitRes (regexp-split #rx"\n" str)])
    (letrec ([goThrough
              (λ (lst hold count)
                (if (null? lst) hold
                    (let ([isRule? (retrRule (car lst))]
                          [isComment? (elimComment (car lst))])
                      (cond (isComment? (goThrough (cdr lst) hold (+ count 1)))
                            (isRule? (goThrough (cdr lst) (append hold `(,isRule?)) (+ count 1)))
                            ((whiteLine? (car lst)) (goThrough (cdr lst) hold (+ count 1)))
                            (else (raise count))))))])
      (with-handlers ([(λ (t) #T)
                       (λ (t) (display "Cannot parse line ")
                              (display t) (newline))])
        (goThrough splitRes '() 1)))))
(define (runMarkov ruleStr inputStr)
  ((interpret (denoteRules ruleStr)) inputStr))
