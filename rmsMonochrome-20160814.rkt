#lang racket
(require racket/draw)

(define (bitmapFrom x) (make-object bitmap% x))
(define (pixelAt x y)
  (lambda (b)
    (let ([pixel (make-shared-bytes 4 0)])
      (begin (send b get-argb-pixels x y 1 1 pixel) pixel))))
(define (rmsPixel x)
  (let ([res (exact-round (sqrt (/ (+ (expt (bytes-ref x 1) 2)
                                      (expt (bytes-ref x 2) 2)
                                      (expt (bytes-ref x 3) 2)) 3)))])
    (bytes (bytes-ref x 0) res res res)))
(define rmsMonochrome
  (lambda (b)
    (let ([bWidth (send b get-width)]
          [bHeight (send b get-height)]
          [res b])
      (begin
        (for ([x (range 0 bWidth)])
          (for ([y (range 0 bHeight)])
            (let* ([pixel ((pixelAt x y) res)]
                   [resP (rmsPixel pixel)])
              (send res set-argb-pixels x y 1 1 resP))))
        res))))
