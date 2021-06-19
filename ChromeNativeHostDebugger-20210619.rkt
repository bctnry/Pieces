#lang racket/base
(require racket/class)
(require racket/gui/base)
(require racket/string)

(define processHost (void))

(define frmMain
  (new frame%
       [label "Chrome Extension Native Host Debugger"]))

(define hpExecutable
  (new horizontal-panel%
       (parent frmMain)
       (stretchable-height #f)))
(define tfExecutable
  (new text-field%
       (label "executable file")
       (parent hpExecutable)))
(define btnOpenExecutable
  (new button%
       (parent hpExecutable)
       (label "Open...")
       (callback (λ (e btn)
                   (let ((get-res (get-file)))
                     (send tfExecutable set-value (or (and get-res (path->string get-res)) ""))
                     )))))
(define btnRunExecutable
  (new button%
       (parent hpExecutable)
       (label "Run")
       (callback (λ (e btn) (runHost e btn)))))
(define btnStopExecutable
  (new button%
       (parent hpExecutable)
       (label "Stop")
       (callback (λ (e btn) (stopHost e btn)))))

(define inputHost (void))
(define outputHost (void))
(define errHost (void))
(define hpInput
  (new horizontal-panel%
       (parent frmMain)
       (stretchable-height #f)))
(define tfInput
  (new text-field%
       (label "input")
       (parent hpInput)))
(define btnInput
  (new button%
       (parent hpInput)
       (label "Input")
       [callback (λ (e btn)
                  (let* ((user-input (string-trim (send tfInput get-value)))
                         (user-input-bytes (string->bytes/utf-8 user-input))
                         (len (bytes-length user-input-bytes))
                         (len-data (integer->bytes len))
                         (data (bytes-append len-data user-input-bytes)))
                    (unless (void? inputHost)
                      (write-bytes data inputHost)
                      (flush-output inputHost))))]))

(define threadOutput (void))
(define hpOutput
  (new vertical-panel%
       (parent frmMain)))
(define tfOutput
  (new text-field%
       (label "output")
       (parent hpOutput)
       (style '(multiple vertical-label))))

(define threadError (void))
(define hpError
  (new vertical-panel%
       (parent frmMain)))
(define tfError
  (new text-field%
       (label "error")
       (parent hpError)
       (style '(multiple vertical-label))))


(define (integer->bytes i)
  (let ((a (modulo i 255))
        (b (modulo (quotient i 256) 256))
        (c (modulo (quotient i (expt 256 2)) 256))
        (d (modulo (quotient i (expt 256 3)) 256)))
    (bytes a b c d)))
(define (bytes->integer b)
  (+ (bytes-ref b 0)
     (* (bytes-ref b 1) 256)
     (* (bytes-ref b 1) (expt 256 2))
     (* (bytes-ref b 1) (expt 256 3))))

(define (stopHost e btn)
  (unless (void? inputHost) (close-output-port inputHost))
  (unless (void? outputHost) (close-input-port outputHost))
  (unless (void? errHost) (close-input-port errHost))
  (unless (void? threadOutput) (kill-thread threadOutput))
  (unless (void? threadError) (kill-thread threadError))
  (unless (void? processHost) (subprocess-kill processHost #t))
  (set! inputHost (void))
  (set! outputHost (void))
  (set! errHost (void))
  (set! threadOutput (void))
  (set! threadError (void))
  (set! processHost (void)))
  
(define (runHost e btn)
  (define-values (sp out in err)
    (subprocess #f #f #f (send tfExecutable get-value)))
  (set! processHost sp)
  (set! inputHost in)
  (set! outputHost out)
  (set! errHost err)
  (set! threadOutput
        (thread (λ ()
                  (for ([i (in-naturals)])
                    #:break (or (void? outputHost) (port-closed? outputHost))
                    (let* ((l (read-bytes 4 out))
                           (len (bytes->integer l))
                           (data (read-bytes len out)))
                      (send tfOutput set-value
                            (string-append
                             (send tfOutput get-value)
                             "\n"
                             (bytes->string/utf-8 data)))
                      )))))
  (set! threadError
        (thread (λ ()
                  (for ([i (in-naturals)])
                    #:break (or (void? errHost) (port-closed? errHost))
                    (send tfError set-value
                          (string-append
                           (send tfError get-value)
                           "\n"
                           (read-line errHost 'any))))))))

(send frmMain show #t)

