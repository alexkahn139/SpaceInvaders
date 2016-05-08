#lang racket
(provide maak-kogels-adt)

(require "teken-adt.rkt")
(require "Abstracties.rkt")

(require "Kogel.rkt")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;
;;Kogels ADT;;
;;;;;;;;;;;;;;

;Dit ADT zal als doel hebben om verschillende kogels te maken en te zorgen dat deze elk de correcte handelingen doen.
(define (maak-kogels-adt)
  (define kogels '())
  (define volgende-kogel 'normaal)
  (define (maak-kogel! posx teken-adt) ;wordt opgeroepen bij schieten
    (let ((nieuwe-kogel (maak-kogel-adt posx volgende-kogel)))
      (set! kogels (cons nieuwe-kogel kogels)) ;maakt dmv van cons cellen een lijst met kogels. Efficienter door van voor te consen.
      ;((nieuwe-kogel 'schiet!) posx teken-adt) ;Zorgt dat de kogel afgeschoten wordt
      ((teken-adt 'nieuwe-kogel!) nieuwe-kogel)
      ((teken-adt 'teken-kogel!) nieuwe-kogel)

      ))

  (define (loop-over-kogels functie) ;Zorgt dat functies over de kogels gemapt kunnen worden.
    (map functie kogels))

  (define (teken! teken-adt)
    (for-each (lambda (kogel-adt)
                ((kogel-adt 'teken!) teken-adt))
              kogels))
  (define (beweeg! teken-adt vloot-adt score-adt power-up huidige-tijd spel)
    (for-each (lambda (kogel-adt)
                ((kogel-adt 'beweeg!) teken-adt dispatch)
                ((kogel-adt 'schietlus) teken-adt vloot-adt dispatch score-adt power-up huidige-tijd spel))
              kogels))
  (define (delete-inactieve-kogels)
    (define (delete-loop lijst reslijst)
      (if (not (null? lijst))
          (if (eq? ((car lijst) 'staat?) 'inactief)
              (delete-loop (cdr lijst) reslijst)
              (delete-loop (cdr lijst) (cons (car lijst) reslijst)))
          (set! kogels (reverse reslijst))))
    (delete-loop kogels '()))
  (define (volgende-kogel! type)
    (set! volgende-kogel type))
  (define (dispatch msg)
    (cond ((eq? msg 'maak-kogel!) maak-kogel!)
          ((eq? msg 'kogels) kogels)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'delete-inactieve-kogels) delete-inactieve-kogels)
          ((eq? msg 'volgende-kogel!) volgende-kogel!)
          ))
  dispatch)
