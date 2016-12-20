#lang racket
(require "teken-adt.rkt")
(require "Abstracties.rkt")
(provide maak-kogel-adt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;
;;Kogel ADT;;
;;;;;;;;;;;;;
;Dit ADT zal de kogel aanmaken.Het zal nadat het afgeschoten is moeten voortbewegen op het scherm.

(define (maak-kogel-adt positie-x type)
  ;Functie om te schieten is weg. Handiger om elke kogel bij aanmaak de juiste x-waarde mee te geven.
  
  ;Een aantal constantes
  ;  (define positie-x 0.5)
  (define positie-y 1)
  (define snelheid 0.03)
  (define staat 'actief)
  
  ;Functies om de posities aan te passen
  (define (set-positie-x! nieuwe-x)
    (set! positie-x nieuwe-x))
  (define (set-positie-y! nieuwe-y)
    (set! positie-y nieuwe-y))
  
  
  ;Deze functie zorgt voor de beweging van de kogel. Indien de kogel voorbij de rand van het scherm komt wordt hij als het ware herladen.
  (define (beweeg! teken-adt kogels-adt)
    (cond ((< 0 positie-y)
           (set-positie-y! (decrement positie-y snelheid)))
          (else (begin (set! staat 'inactief)
                       ((teken-adt 'delete-kogel!) dispatch-kogel)
                       ((kogels-adt 'delete-inactieve-kogels))
                       ))))
  
  ;Deze functie zorgt voor het verwijderen. teken-adt krijgt de boodschap om de kogel te deleten.
  (define (delete! teken-adt)
    ((teken-adt 'delete-kogel!) dispatch-kogel
                                ))
  
  ;Laat de kogel na een botsing zowel inactief worden als van het scherm te verdwijnen. In verdere fase kan dan naar het type kogel gekeken worden.
  (define (geraakt! object teken-adt vloot-adt kogels-adt score-adt huidige-tijd spel)
    (set! staat 'inactief)
    (delete! teken-adt)
    ((object 'geraakt!) dispatch-kogel teken-adt vloot-adt score-adt kogels-adt huidige-tijd spel)
    ;((kogels-adt 'delete-inactieve-kogels))
    
    )
  
  ;Gaat na of er een botsing is.
  (define (schietlus teken-adt vloot-adt kogels-adt score-adt power-up huidige-tijd spel) ;power-up
    (define (schiet-op object)
      (if (eq? staat 'actief)
          (cond ((check-geraakt? object dispatch-kogel)
                 (geraakt! object teken-adt vloot-adt kogels-adt score-adt huidige-tijd spel))
                )
          'ok))
    ((vloot-adt 'loop-over-vloot) schiet-op)
    (if (eq? (power-up 'staat) 'actief)
        (cond ((check-geraakt? power-up dispatch-kogel)
               (geraakt! power-up teken-adt vloot-adt kogels-adt score-adt huidige-tijd spel)))
        'ok)); Zorgt ervoor dat elke alien afgegaan wordt.
  
  ;Zorgt ervoor dat het teken-adt het object tekent.
  (define (teken! teken-adt)
    ((teken-adt 'teken-kogel!) dispatch-kogel))
  
  (define (dispatch-kogel msg)
    (cond
      ((eq? msg 'x!) set-positie-x!)
      ((eq? msg 'y!) set-positie-y!)
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'beweeg!) beweeg!)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'staat?) staat) ;Geeft de staat van de kogel terug
      ((eq? msg 'geraakt!) geraakt!)
      ((eq? msg 'type) type)
      ((eq? msg 'schietlus) schietlus)
      ))
  dispatch-kogel)
