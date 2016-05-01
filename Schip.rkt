#lang racket
(require "teken-adt.rkt")
(require "Abstracties.rkt")
(provide maak-schip-adt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;
;;Schip ADT;;
;;;;;;;;;;;;;
;Het Schip-adt zal het schip aanmaken. Het zal ervoor kunnen zorgen dat het kan voortbewegen.

(define (maak-schip-adt)
  ;Een aantal vaste beginwaarden
  (define positie-x 0.5)
  (define positie-y 1)
  (define snelheid 0.06)
  
  ;Zorgt dat we een nieuwe positie kunnen geven aan het schip
  ;Geen set-positie-y! nodig aangezien het schip enkel op de x-as beweegt
  (define (set-positie-x! nieuwe-x)
    (set! positie-x nieuwe-x))
  
  ;Het schip zal moeten bewegen op basis van invoer van het toetsenbord. Ook moet er gezien worden dat het schip niet uit het scherm kan komen
  (define (beweeg! richting)
    (cond
      ((eq? richting 'links)
       (if (< 0 positie-x)
           (set-positie-x! (decrement positie-x snelheid))
           positie-x))
      ((eq? richting 'rechts)
       (if (> 1 positie-x)
           (set-positie-x! (increment positie-x snelheid))
           positie-x))))
  
  ;Zorgt ervoor dat het teken-adt het object tekent.
  (define (teken! teken-adt)
    ((teken-adt 'teken-schip!) dispatch-schip))
  
  ;Dispatch er met dit object geïnterageerd kan worden.
  (define (dispatch-schip msg)
    (cond
      ((eq? msg 'x!) set-positie-x!)
      ((eq? msg 'x) positie-x)
      ((eq? msg 'beweeg!) beweeg!)
      ((eq? msg 'teken!) teken!)))
  
  dispatch-schip)
