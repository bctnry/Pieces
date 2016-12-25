#lang racket
(require net/base64) ; I don't want to roll my own base64 decoder
; The idea is from:
;     https://www.codewars.com/kata/minimal-squarewave-pcm-synthesizer

(define baseFrequency 440) ; A above central C
; this frequency is defined to be (note A 0).
(define centralCFrequency (/ baseFrequency (expt 2 (/ 9 12))))
(struct note (name octave) #:transparent)
(define (toNat/NoteName notename)
  (case notename
    ((C) 0) ((Cs) 1) ((D) 2) ((Ds) 3)
    ((E) 4) ((F) 5) ((Fs) 6) ((G) 7)
    ((Gs) 8) ((A) 9) ((As) 10) ((B) 11)))
(define (<=/Note from to)
  (or (< (note-octave from) (note-octave from))
      (and (= (note-octave from) (note-octave to))
           (<= (toNat/NoteName (note-name from))
               (toNat/NoteName (note-name to))))))
(define (halfsteps from to)
  (cond ((= (note-octave from) (note-octave to))
         (- (toNat/NoteName (note-name to))
            (toNat/NoteName (note-name from))))
        (else (+ (* (- (note-octave to) (note-octave from)) 12)
                 (halfsteps from (note (note-name to) (note-octave from)))))))
(define (toFreq/Note note)
  ; I'm using equal temperament here. the calculating formula is from:
  ;     https://en.wikipedia.org/wiki/Equal_temperament
  ; at the "Comparison to just intonation" section.
  (* centralCFrequency ; calculation based on central C
     (expt 2 (note-octave note)) ; raise to the same octave
     (expt 2 (/ (toNat/NoteName (note-name note)) 12)))) ; raise to the note itself
; this is not based on the #<value:baseFrequency>. change it in the future?

; 1. the frequency is the speed of variation; for example, the list '(8 9 10 11 12
;    13 14 15 14 13 ... 9 8 7 6 5 4 3 2 1 0 1 2 3 .... 7 8 9 ...) has a lower
;    frequency than '(8 10 12 14 14 12 10 8 6 4 2 0 2 4 ...)
; 2. the duration is "sampling rate * the number of samples"; for example, a list of
;    44100 numbers means 1 second if using a 44.1khz sampling rate, 2 seconds for
;    a 22050 hz sampling rate and so on...
; 3. so a cycle actually lasts (samplingRate / freq) samples.
(define samplingRate 44100) ; 44.1khz

; (-> (-> freq amp sampletime) (-> note amp duration))
; where amp = (between/c? 0.0 1.0),
;       sampletime = exact-nonnegative-integer,
;       duration = number
; and the amp represents the maximum amp the sample will have.
(define (mkSynth f)
  (λ (note amp duration)
    (for/list ([i (range 0 (exact-round (* samplingRate duration)))])
      (f (toFreq/Note note) amp i))))
(define example/squareWave (mkSynth
    (λ (freq amp t)
      (let ([cycle (exact-round (/ samplingRate freq))])
        ; not sure if I'm doing right with the line above.
        ; 0 ~ 1 <-f-> 128 ~ 255 =====>
        ; f = +128 exact-floor *127
        (if (<= (remainder t cycle) (/ cycle 2))
            (+ 128 (exact-floor (* 127 amp)))
            (exact-floor (* 128 amp)))))))
; the calculating is actually kind of tricky here.

(define example/wavHeader
  (base64-decode #"UklGRrRKQwBXQVZFZm10IBAAAAABAAEARKwAAESsAAABAAgAZGF0YZBKQwA="))
; this is the 1-channel 8bit pcm .wav header copied from the kata

; 140 bpm for a time signature of 4/4, that is 60/280 seconds for an eighth note.
(define noteLen (/ 60 (* 140 2)))
(define resultA
  ; an arpeggio of Dm7 chord, which is D-F-A-C.
  (bytes-append example/wavHeader
                (list->bytes (example/squareWave (note 'D 0) 0.5 noteLen))
                (list->bytes (example/squareWave (note 'F 0) 0.5 noteLen))
                (list->bytes (example/squareWave (note 'A 0) 0.5 noteLen))
                (list->bytes (example/squareWave (note 'C 1) 0.5 noteLen))
                ; this note belongs to the next octave.
                ))
(define (toFile x a)
  (let ([f (open-output-file x #:mode 'binary #:exists 'replace)])
    (write-bytes a f)))
; replace the F:\\ExampelA.WAV thing with the path you want to save the file to
(toFile "F:\\ExampleA.WAV" resultA)

; to turn this piece of code into something that's really usable you'll have to
; add a lot of stuff including:
; 1. a parameterized header-maker. sometimes 1channel 8bit will not be enough
;    if you want to make some modulation & stuff or if you want to have a
;    better quality.
; 2. a way to convert a note to a (-> amp duration data), and a way to combine
;    these functions, because monophony melodies are not too interesting. the
;    #<function:mkSynth> may not be the good choise for this; you might have to
;    make your own version of mkSynth which is redesigned and suitable for more
;    advanced uses.
; 3. a (-> timeSignature BPM noteLength).
; 4. a way to support different wave shape.
; 5. LFO for amp modulation
; 6. all kinds of filters & stuff, and LFOs for them.
