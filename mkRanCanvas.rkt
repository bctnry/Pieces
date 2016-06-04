#lang racket
(require racket/draw)
(define (random-color)
  (let ([r (- (random 1 256) 1)]
        [g (- (random 1 256) 1)]
        [b (- (random 1 256) 1)])
    (make-object color% r g b)))
(define (random-gray)
  (let ([c (- (random 1 256) 1)])
    (make-object color% c c c)))
(define (random-draw dc height width colorgen)
  (let* ([x (- (random 1 width) 1)]
         [y (- (random 1 height) 1)]
         [w (let ([w_ (random 1 width)])
              (if (> (+ w_ x) width) (- width x) w_))]
         [h (let ([h_ (random 1 width)])
              (if (> (+ h_ y) height) (- height y) h_))])
    (begin
      (send dc set-brush (new brush% [color (colorgen)]))
      (send dc set-pen (new pen% [style 'transparent]))
      (send dc draw-rectangle x y w h))))
(define (randomD dc height width colorgen times)
  (if (= times 1) (random-draw dc height width colorgen)
      (begin (random-draw dc height width colorgen)
             (randomD dc height width colorgen (- times 1)))))
(define (background dc height width c)
  (begin
    (send dc set-brush (new brush% [color c]))
    (send dc set-pen (new pen% [style 'transparent]))
    (send dc draw-rectangle 0 0 height width)))
(define (mkRanCanvas height width colorgen t)
  (let* ([canvas (make-bitmap height width)]
         [dc (new bitmap-dc% [bitmap canvas])])
    (begin (background dc height width (make-object color% 255 255 255))
           (randomD dc height width colorgen t)
           canvas)))
; use (mkRanCanvas height width color-generator times-of-drawing)
; where color-generator : (-> color%)
