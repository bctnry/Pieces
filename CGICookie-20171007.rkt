; for reading cookie

(define (get-cookie)
  ; require racket/string
  (let ((cookiestring (getenv "HTTP_COOKIE")))
    (for/list ([i (string-split cookiestring ";")])
      (let ((p (string-split (string-trim i " ") "=")))
        (cons (car p) (cadr p))))))
(define (extract-cookie key) (cdr (assoc key (get-cookie))))
