#lang racket
(provide maak-rotator-adt)

(require "teken-adt.rkt")
(require "Abstracties.rkt")




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;Maakt het adt aan met de selector.
(define (maak-rotator-adt)
  (define staat 0)
  (define positie-x 0.45) ;posities zo gekozen dat het mooi uitkwam
  (define positie-y 0.4)
  
  (define (up! teken-adt) ;reageert als je met het pijltje naar boven gaat
    (if (> staat 0)
        (begin
          (set! staat (- staat 1))
          (set! positie-y (- positie-y 0.18))) ;zorgt dat het mooi uitkomt, is omdat het menu 1 foto is
        'ok)
    (teken! teken-adt))
  
  (define (down! teken-adt)
    ;reageert als je met het pijltje naar boven gaat
    (if (< staat 2)
        (begin
          (set! staat (+ staat 1))
          (set! positie-y (+ positie-y 0.18))) ;zorgt dat het mooi uitkomt, is omdat het menu 1 foto is
        'ok)
    (teken! teken-adt))
  
  ;Geeft mee aan het teken-adt om alles te tekenen
  (define (teken! teken-adt)
    ((teken-adt 'teken-rotator!) dispatch-rotator))
  (define (dispatch-rotator msg)
    (cond
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'staat) staat)
      ((eq? msg 'up!) up!)
      ((eq? msg 'down!) down!)))
  dispatch-rotator)
