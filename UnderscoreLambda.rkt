#lang racket
(define (! x)
  (define (elem? eqp x l)
    (match l ['() #F]
      [(cons y ys) (or (eqp x y) (elem? eqp x ys))]))
  (define (substSingl eqp s t l)
    (cond ((eqp l s) t)
          ((pair? l)
           (cons (if (eqp (car l) s) t (car l))
                 (substSingl eqp s t (cdr l))))
          (else l)))
  (define (lambdaExpand x n)
    (define (mkLambda tempvar body) (cons 'lambda (list (list tempvar) body)))
    (cond ((list? x)
           (cond ((elem? eq? '_ x)
                  (let ([tempsymbol (string->symbol (string-append "t" (number->string n)))])
                    (map (lambda (t2) (lambdaExpand t2 (+ n 1)))
                         (mkLambda tempsymbol (substSingl eq? '_ tempsymbol x)))))
                 (else
                  (map (lambda (t2) (lambdaExpand t2 n)) x))))
           (else x)))
  (eval (lambdaExpand x 0)))
; ((! '(_ 2 3)) +) ==> 5
; ((! '(+ _ _)) 3) ==> 6
; that is: (define (double x) (+ x x))
; could be: (define double (! '(+ _ _)))
; which is expaned to: (define double (lambda (t0) (+ t0 t0)))
; 2016.6.4
