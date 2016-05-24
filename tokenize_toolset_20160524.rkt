#lang typed/racket
(define _UPPERCASE "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define _LOWERCASE "abcdefghijklmnopqrstuvwxyz")
(define _NUMBERS "0123456789")
(define _SYMBOLS "~!@#$%^&*()-_=+`\\|;:'\",.<>/?")
(define _WHITESPACE " \t")
(define _NEWLINE (list->string (list #\newline #\return)))
(define _CHARS (string-append _UPPERCASE _LOWERCASE))
(define _ALPHANUM (string-append _CHARS _NUMBERS))
(define _PRINTSYMBOLS (string-append _SYMBOLS _ALPHANUM))
(define-type ParseM (U #F (List String String (Listof Any))))
(define-type ParseC (-> String ParseM))
(define-type AsClause (-> String Any))

(: elem (All (A) (-> (-> A A Boolean) A (Listof A) Boolean)))
(define (elem eqpred x l)
  (match l ['() #F] [(cons y ys) (if (eqpred x y) #T (elem eqpred x ys))]))

(: isPrefix? (All (A) (-> (-> A A Boolean) (Listof A) (Listof A) Boolean)))
(: restOfPrefix (All (A) (-> (Listof A) (Listof A) (Listof A))))
(define (isPrefix? eqpred prel l)
  (match prel ['() #T]
    [(cons x xs) (match l ['() #F] [(cons y ys)
                                    (if (eqpred x y) (isPrefix? eqpred xs ys) #F)])]))
(define (restOfPrefix prel l) (drop l (length prel)))

(: _AS (-> Any (-> Any Any)))
(define (_AS x) (lambda (t) x))

; _ZERO :: a -> m b
(: _ZERO ParseC)
(define (_ZERO t) (list "" t '()))

; _ONEOF :: str -> a -> m b
(: _ONEOF (-> String AsClause ParseC))
(define (_ONEOF str as)
  (let ([strchl (string->list str)])
    (lambda (t)
      (let* ([tl (string->list t)]
             [tlx (car tl)]
             [tls (cdr tl)])
        (if (elem eqv? tlx strchl)
            (list (list->string (list tlx))
                  (list->string tls)
                  (list (as (list->string (list tlx)))))
            #F)))))

(: _MULTIOF (-> String AsClause ParseC))
(define (_MULTIOF str as)
  (let ([strchl (string->list str)])
    (lambda (t)
      (let* ([tl (string->list t)]
             [tlx (takef tl (lambda (temp) (elem eqv? temp strchl)))]
             [tls (dropf tl (lambda (temp) (elem eqv? temp strchl)))])
        (if (null? tlx) #F
            (list (list->string tlx)
                  (list->string tls)
                  (list (as (list->string tlx)))))))))

(: _EXCLUDE (-> String String String))
(define (_EXCLUDE strex strl)
  (let ([strexchl (string->list strex)]
        [strchl (string->list strl)])
    (match strchl ['() strl]
      [(cons ch chs)
       (if (elem eqv? ch strexchl)
           (_EXCLUDE strex (list->string chs))
           (string-append (list->string (list ch)) (_EXCLUDE strex (list->string chs))))])))

(: _WITH (-> String String String)) (define _WITH string-append)

(: _OR (-> ParseC ParseC ParseC))
(define (_OR pc1 pc2)
  (lambda (t)
    (let ([pc1res (pc1 t)])
      (if (eq? pc1res #F) (pc2 t) pc1res))))

(: _EXACT (-> String AsClause ParseC))
(define (_EXACT str as)
  (let ([strchl (string->list str)])
    (lambda (t)
      (let* ([tl (string->list t)]
             [prefix? (isPrefix? eqv? strchl tl)])
        (if prefix? (list str (list->string (restOfPrefix strchl tl))
                          (list (as str)))
            #F)))))

(: _SAME (-> String String))
(define (_SAME x) x)

; >>= :: m a -> (a -> m b) -> m b
(: >>= (-> ParseM ParseC ParseM))
(define (>>= t c)
  (match t
    [#F #F]
    [(list res1 res2 tokenl)
     (match (c res2)
       [#F #F]
       [(list nres1 nres2 tokenl2)
        (list (string-append res1 nres1)
              nres2
              (append tokenl tokenl2))])]))

; pass to 2 parserc and forget the effect of the first.
(: >> (-> ParseC ParseC ParseC))
(define (>> x y)
  (lambda (t)
    (match (x t)
      [#F #F]
      [(list res1 res2 _)
       (match (y res2)
         [#F #F]
         [(list nres1 nres2 tokenl)
          (list (string-append res1 nres1)
                nres2 tokenl)])])))

(: << (-> ParseC ParseC ParseC))
(define (<< x y)
  (lambda (t)
    (match (x t)
      [#F #F]
      [(list res1 res2 tokenl)
       (match (y res2)
         [#F #F]
         [(list nres1 nres2 _)
          (list (string-append res1 nres1)
                nres2 tokenl)])])))

(: _AND (-> ParseC ParseC ParseC))
(define (_AND x y) (lambda (t) (>>= (x t) y)))

(: _ORsx (-> (Listof ParseC) ParseC ParseC))
(define (_ORsx l endc)
  (match l ['() endc] [(cons x xs) (_OR x (_ORsx xs endc))]))
(: _ORs (-> (Listof ParseC) ParseC))
(define (_ORs l) (_ORsx l _ZERO))

(: _ANDsx (-> (Listof ParseC) ParseC ParseC))
(define (_ANDsx l endc)
  (match l ['() endc] [(cons x xs) (_AND x (_ANDsx xs endc))]))
(: _ANDs (-> (Listof ParseC) ParseC))
(define (_ANDs x) (_ANDsx x _ZERO))

(: mkFix (-> ParseC ParseC ParseC))
(define (mkFix fstc endc)
  (lambda (t)
    (let ([res (fstc t)])
      (match res [#F (endc t)]
        [_ (>>= res (mkFix fstc endc))]))))

(: _FAIL ParseC)
(define (_FAIL x) #F)
