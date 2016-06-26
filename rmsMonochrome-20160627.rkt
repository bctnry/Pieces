#lang racket
(require racket/draw)
(require pict)
(define (rOf x) (bytes-ref x 1))
(define (gOf x) (bytes-ref x 2))
(define (bOf x) (bytes-ref x 3))
(define (avg x)
  (define (psum p x h)
    (match x ['() h] [(cons x xs) (psum p xs (+ (* p x) h))]))
  (psum (/ (length x)) x 0))
(define (rms x) (* 1.0 (sqrt (avg (map (lambda (t) (expt t 2)) x)))))
(define (intrms x) (exact-round (rms x)))
(define (toMonochrome_ b hold)
  (if (bytes=? b #"") hold
      (let* ([pixel (subbytes b 0 4)]
             [rmsOf (intrms (list (rOf pixel) (gOf pixel) (bOf pixel)))]
             [res (bytes (bytes-ref pixel 0) rmsOf rmsOf rmsOf)])
        (toMonochrome_ (subbytes b 4)
                       (bytes-append hold res)))))
(define (toMonochrome b)
  (if (send b is-color?)
      (pict->bitmap (argb-pixels->pict
        (toMonochrome_ (pict->argb-pixels (bitmap b)) #"")
        (send b get-width)))
      b))
