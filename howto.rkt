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

  (define (install! teken-adt)
    ((teken-adt 'maak-how-to!))
    ((teken-adt 'delete-menu!))
    ((teken-adt 'delete-rotator!))
    )
  (define (uninstall! teken-adt)
    ((teken-adt 'delete-how-to!))
    ((teken-adt 'maak-menu!))
    ((teken-adt 'maak-rotator!))
    )

  (define (dispatch-how-to msg)
    (cond
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'install!) install!)
      ((eq? msg 'uninstall!) uninstall!)
      ))
  dispatch-how-to)
