#lang racket
; lambda calculus, cek style.

; Closure : Term * Env
; Env     : Var -> Closure

;
; Var.a env K
; --------------------------------------
; env.lookup(a).term env.lookup(a).env K
;
; (A B) env K
; -----------
; A env (2 B env K)
;
; λx.A env (2 B env' K)
; ---------------------
; B env' (! λx.A env K)
;
; A env (! λx.B env' K)
; ------------------------
; B env'+(x=A*env) K

(define (eval_ c e k) (let*
  ((ext (λ (v c k) (λ (t) (if (equal? t v) c (k t)))))
   (var cadr) (body caddr) (term cadr) (env caddr) (kont cadddr)
   (func cadr) (arg caddr)
   (s (λ (c e k)
        (case (car c)
          ((V) (let ([c (e c)]) `(,(car c) ,(cdr c) ,k)))
          ((A) `(,(func c) ,e (2 ,(arg c) ,e ,k)))
          ((L) (case (car k)
                 ((N) `(,c ,e ,k))
                 ((2) `(,(term k) ,(env k) (! ,c ,e ,(kont k))))
                 ((!) `(,(body (term k))
                        ,(ext (var (term k)) (cons c e) (env k))
                        ,(kont k))))))))
   (? (λ (c e k) (and (equal? (car c) 'L) (equal? k '(N))))))
  (if (? c e k) `(,c ,e ,k) (apply eval_ (s c e k)))))

(let ([r (eval_ '(A (A (L (V "X") (L (V "Y") (V "X"))) (L (V "Z") (V "Z")))
                    (L (V "A") (V "A"))) (λ (t) (error "")) '(N))])
  (equal? (car r) '(L (V "Z") (V "Z"))))
