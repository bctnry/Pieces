#lang racket
(require racket/gui)

(define board '())
(define board-width 25)
(define board-height 25)
(define cell-size 20)
(define time-step 50)
(define (make-board width height)
  (for/vector ([i (in-range height)])
    (make-vector width 0)))
(set! board (make-board board-width board-height))
(define (reset-board) (set! board (make-board board-width board-height)))
(define (board-at x y)
  (vector-ref (vector-ref board y) x))
(define (set-board-at board x y)
  (vector-set! (vector-ref board y) x 1))
(define (clear-board-at board x y)
  (vector-set! (vector-ref board y) x 0))
(define (toggle-board-at board x y)
  (let ((v (vector-ref board y)))
    (vector-set! v x (- 1 (vector-ref v x)))))
(define (toggle-board x y) (toggle-board-at board x y))
(define (clip120 t)
  (cond ((= (car t) -1) (clip120 (cons (- board-width 1) (cdr t))))
        ((= (car t) board-width) (clip120 (cons 0 (cdr t))))
        ((= (cdr t) -1) (clip120 (cons (car t) (- board-height 1))))
        ((= (cdr t) board-height) (clip120 (cons (car t) 0)))
        (#t t)))
(define (neighbourhood i j)
  (map clip120 (list (cons (- i 1) (- j 1)) (cons i (- j 1)) (cons (+ i 1) (- j 1))
                     (cons (- i 1) j) (cons (+ i 1) j)
                     (cons (- i 1) (+ j 1)) (cons i (+ j 1)) (cons (+ i 1) (+ j 1)))))
(define (step)
  (let ((result (make-board board-width board-height)))
    (for ([j (in-range 0 board-height)])
      (for ([i (in-range 0 board-width)])
        (let ((neighbourhoodalive
               (apply + (map (λ (t) (board-at (car t) (cdr t))) (neighbourhood i j)))))
          (if (= 0 (board-at i j))
              (if (= neighbourhoodalive 3)
                  (set-board-at result i j)
                  (clear-board-at result i j))
              (if (or (= neighbourhoodalive 2) (= neighbourhoodalive 3))
                  (set-board-at result i j)
                  (clear-board-at result i j))))))
    (set! board result)))

(define frmMain
  (new frame%
       (label "Game of Life")
       (stretchable-width #f)
       (stretchable-height #f)))
(define canvasMain
  (new (class canvas% (super-new)
         (define timer
           (new timer%
                (interval time-step)
                (notify-callback
                 (λ ()
                   (begin (step) (send this on-paint))))))
         (define/public (start) (send timer start time-step))
         (define/public (stop) (send timer stop))
         (define dc (send this get-dc))
         (define/override (on-paint)
           (for* ([j (in-range 0 board-height)] [i (in-range 0 board-width)])
             (begin
               (send dc set-brush
                     (if (= 0 (board-at i j)) "black" "green")
                     'solid)
               (send dc draw-ellipse
                     (* cell-size i) (* cell-size j)
                     cell-size cell-size))))
         (define/override (on-event e)
           (when (send e button-down? 'left)
             (begin
               (let ((t (clip120 (cons (quotient (send e get-x) cell-size)
                                       (quotient (send e get-y) cell-size)))))
                 (toggle-board (car t) (cdr t)))
               (send this on-paint))))
         )
       (parent frmMain)
       (min-width (* cell-size board-width))
       (min-height (* cell-size board-height))))
(define btnRun
  (new button%
       (label "Run")
       (parent frmMain)
       (callback
        (λ (btn e)
          (send canvasMain start)
          ))))
(define btnPause
  (new button%
       (label "Pause")
       (parent frmMain)
       (callback
        (λ (btn e)
          (send canvasMain stop)
          ))))
(define btnConfig
  (new button%
       (label "Configure")
       (parent frmMain)
       (callback
        (λ (btn e)
          (send frmConfig show #t)))))

(define (reset-everything)
  (send canvasMain stop)
  (send (send canvasMain get-dc) clear)
  (set! board (make-board board-width board-height))
  (send canvasMain min-client-width (* cell-size board-width))
  (send canvasMain min-client-height (* cell-size board-height))
  (send canvasMain on-paint)
  )
(define frmConfig
  (new frame%
       (label "Configuration")
       (stretchable-width #f)
       (stretchable-height #f)))
(define gpBoard
  (new group-box-panel%
       (label "Board & Display")
       (parent frmConfig)))
(define tfBoardWidth
  (new text-field%
       (label "Width: ")
       (parent gpBoard)))
(define tfBoardHeight
  (new text-field%
       (label "Height: ")
       (parent gpBoard)))
(define tfCellSize
  (new text-field%
       (label "Cell size: ")
       (parent gpBoard)))
(define gpTime
  (new group-box-panel%
       (label "Time")
       (parent frmConfig)))
(define tfTimestep
  (new text-field%
       (label "Timestep: ")
       (parent gpTime)))
(define btnSave
  (new button%
       (label "Save")
       (parent frmConfig)
       (callback
        (λ (btn e)
          (let ((newBoardWidth (string->number (send tfBoardWidth get-value)))
                (newBoardHeight (string->number (send tfBoardHeight get-value)))
                (newCellSize (string->number (send tfCellSize get-value)))
                (newTimestep (string->number (send tfTimestep get-value))))
            (begin
              (unless (= board-width newBoardWidth) (set! board-width newBoardWidth))
              (unless (= board-height newBoardHeight) (set! board-height newBoardHeight))
              (unless (= cell-size newCellSize) (set! cell-size newCellSize))
              (unless (= time-step newTimestep) (set! time-step newTimestep))
              (reset-everything)))))))
(send tfBoardWidth set-value (number->string board-width))
(send tfBoardHeight set-value (number->string board-height))
(send tfCellSize set-value (number->string cell-size))
(send tfTimestep set-value (number->string time-step))
(send (send canvasMain get-dc) set-pen "white" 0 'transparent)
(send frmMain show #t)
(reset-everything)
