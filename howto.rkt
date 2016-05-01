#lang racket



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(require "teken-adt.rkt")
(require "Abstracties.rkt")
(provide maak-how-to-adt)

(define (maak-how-to-adt)
  (define positie-x 0.3)
  (define positie-y 0.3)
  (define (teken! teken-adt)
    ((teken-adt 'teken-how-to!) dispatch-how-to))
  (define (dispatch-how-to msg)
    (cond
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)))
  dispatch-how-to)
