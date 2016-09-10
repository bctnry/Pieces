#lang racket
(require racket/draw)
(require pict)

(define (countP bitm)
  (let ([blackPixelCount 0]
        [w (send bitm get-width)]
        [h (send bitm get-height)]
        [dc (send bitm make-dc)])
    (begin
      (for ([x (range 0 w)])
        (for ([y (range 0 h)])
          (let ([pixelC (make-object color%)])
            (begin (send dc get-pixel x y pixelC)
                   (when (and (= 0 (send pixelC red))
                              (= 0 (send pixelC green))
                              (= 0 (send pixelC blue)))
                     (set! blackPixelCount (add1 blackPixelCount)))))))
      (/ blackPixelCount (* w h)))))
(define (scale f renewal list)
  (map (renewal (/ (apply max (map f list)))) list))
(define (rankAt fontSize)
  (scale cadr
    (λ (s) (λ (t) (cons (car t) (cons (* s (cadr t)) (cddr t)))))
    (sort
      (for/list ([i (string-append "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                                   "abcdefghijklmnopqrstuvwxyz"
                                   "`~!@#$%^&*()_+=-0987654321"
                                   "{}[]:\";'<>?,./|\\")])
        (let ([bitm (pict->bitmap
                    (text (make-string 1 i)
                          (make-object font% fontSize 'modern
                            'normal 'normal #F 'unsmoothed #F 'aligned)))])
          `(,i ,(* 1.0 (countP bitm)))))
      < #:key cadr)))
(define (eqClasses key l)
  (define (eqClasses_ key l currentKey hold)
    (if (list? l)
        (cond ((null? l) (list hold))
              (else (if (equal? currentKey (key (car l)))
                        (eqClasses_ key (cdr l) currentKey
                                    (append hold (list (car l))))
                        (cons hold (eqClasses_ key (cdr l)
                                               (key (car l)) (list (car l)))))))
        (error "Not a list.")))
  (eqClasses_ key l (key (car l)) (list (car l))))
(define (avgBrightness bitm)
  (define (avg t)
    (/ (+ (bytes-ref t 1) (bytes-ref t 2) (bytes-ref t 3)) 3))
  ; change the (avg pixelBuffer) to (rmsPixel pixelBuffer)
  ; if you prefer rms...
  (define (rmsPixel x)
    (exact-round (sqrt (/ (+ (expt (bytes-ref x 1) 2)
                             (expt (bytes-ref x 2) 2)
                             (expt (bytes-ref x 3) 2)) 3))))
  (let ([w (send bitm get-width)]
        [h (send bitm get-height)]
        [pixelBuffer (bytes 0 0 0 0)]
        [res 0])
    (begin
      (for ([i (range 0 w)])
        (for ([j (range 0 h)])
          (begin
            (send bitm get-argb-pixels i j 1 1 pixelBuffer)
            (set! res (+ res (avg pixelBuffer))))))
      (exact-round (/ res (* w h))))))
(define (bitmapFrom t) (make-object bitmap% t))
(define (subBitmap bitm x y w h)
  (let ([preres (make-bytes (* w h 4) 0)])
    (begin (send (send bitm make-dc)
                 get-argb-pixels x y w h preres)
           ; this might be slow.
           (pict->bitmap (argb-pixels->pict preres w)))))
(define (divide bitm wSize hSize)
  (let ([bitmW (send bitm get-width)]
        [bitmH (send bitm get-height)])
    (let ([divW (/ bitmW wSize)]
          [divH (/ bitmH hSize)])
      (if (or (exact-positive-integer? wSize)
              (exact-positive-integer? hSize)
              (exact-positive-integer? divW)
              (exact-positive-integer? divH))
          (for/list ([y (range 0 bitmH hSize)])
            (for/list ([x (range 0 bitmW wSize)])
              (subBitmap bitm x y wSize hSize)))
          (error "Wrong parameter specified.")))))
(define (getBrightness/divide t mW mH)
  (let ([t (divide (bitmapFrom t) mW mH)])
    (for/list ([i t])
      (for/list ([j i])
        (avgBrightness j)))))
(define (splitRank at)
  (let* ([ranks (eqClasses cadr (rankAt at))]
         [step (exact-round (/ 256 (length ranks)))]
         [degrees (range step 256 step)])
    (cons (list 0 #\space)
          (for/list ([i ranks] [d degrees])
            (cons d (map car i))))))
(define (classify cList t)
  (cond ((null? cList) #F)
        ((null? (cdr cList)) (cdar cList))
        (else (if (> (caadr cList) t) (cdar cList)
                  (classify (cdr cList) t)))))
(define (ranSecl l)
  (let ([n (random 1 (add1 (length l)))])
    (car (drop l (sub1 n)))))
(define (decide at t mW mH)
  (let ([brightnessL (getBrightness/divide t mW mH)]
        [ranks (splitRank at)])
    (map list->string (for/list ([i brightnessL])
      (for/list ([j i])
        (ranSecl (classify ranks j)))))))
; bitmap2ascii.
; 20160911
; decide characters using the courier font.
; uses (decide fontSize bitmapPath mW mH)
; where mW is the width of "cell" that each character represents
;   and mH ------ height ---------------------------------------
; the width of the bitmap must be dividable by mW, and the height
; must be dividable by mH.
