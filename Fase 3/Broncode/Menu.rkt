#lang racket
(provide maak-menu-adt)

(require "teken-adt.rkt")
(require "Abstracties.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;; Menu ADT ;;
;;;;;;;;;;;;;;
(define (maak-menu-adt)
  ;Een aantal constantes en berekende waarden
  (define positie-x 0.60)
  (define positie-y 0.4)
  (define staat 'pauze)
  
  ;geeft aan het teken-adt mee dat het menu getekend moet worden
  (define (teken! teken-adt)
    ((teken-adt 'teken-menu!) dispatch-menu))
  
  ;geeft aan het teken-adt mee dat het menu gedeletet mag worden
  (define (delete! teken-adt)
    ((teken-adt 'delete-menu!)))
  
  ;dient om de staat van het spel in op te slaan
  (define (staat! nieuwe-staat)
    (set! staat nieuwe-staat))
  
  (define (install! teken-adt)
    ((teken-adt 'maak-rotator!))
    ((teken-adt 'maak-menu!))
    ((teken-adt 'verwijder-spelelementen!))
    )
  (define (uninstall! teken-adt)
    ((teken-adt 'delete-rotator!))
    ((teken-adt 'delete-menu!))
    ((teken-adt 'herteken-spelelementen!))
    )
  
  ;Dispatch functie voor interactie met het adt
  (define (dispatch-menu msg)
    (cond
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'staat) staat)
      ((eq? msg 'staat!) staat!)
      ((eq? msg 'delete!) delete!)
      ((eq? msg 'install!) install!)
      ((eq? msg 'uninstall!) uninstall!)
      (else (display "error, wrong msg " msg))
      ))
  dispatch-menu)
