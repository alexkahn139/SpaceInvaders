#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Random functie werkt pas vanaf Racket 6.4!!!

;;;;;;;;;;;;;;;;;
;;Power-Ups ADT;;
;;;;;;;;;;;;;;;;;
(provide maak-power-up-adt)
(require "teken-adt.rkt")
(require "Abstracties.rkt")

(define (maak-power-up-adt positie-x positie-y teken-adt)
  (define timer 0)
  (define levens 1)
  (define staat 'inactief)
  (define random-nummer (random 1 10000))

  ;Hulpfuncties
  ;Om nieuwe posities mee te geven
  (define (set-positie-x! nieuwe-x)
    (set! positie-x nieuwe-x))
  (define (set-positie-y! nieuwe-y)
    (set! positie-y nieuwe-y))
  ;Om de objecten te tekenen of te verwijderen
  (define (teken! teken-adt)
    ((teken-adt 'teken-power-up!) dispatch-power-up kleur)
    )
  (define (delete! teken-adt)
    ((teken-adt 'delete-power-up!))
    ;(deactiveer!)
    (set! random-nummer (+ random-nummer (random 5000 20000))))
  (define (reset!)
    (deactiveer!)
    (set! id (random 1 4)) ;kiest een getal tussen 1 en 3
    (set-positie-x! (/ (random 1 10) 10))
    (set-positie-y! (/ (random 5 10) 10)))

  (define (geraakt! kogel-adt teken-adt vloot-adt score-adt kogels-adt huidige-tijd spel)
    (cond
      ((eq? kleur "yellow")
       ((vloot-adt 'stop!)))
      ((eq? kleur "red")
       ((kogels-adt 'volgende-kogel!) 'speciaal))
      ((eq? kleur "red")
       ((vloot-adt 'versnel!) 2)))
    (set! timer huidige-tijd)
    (set! staat 'bezig)
    (delete! teken-adt)
    ;(reset!)
    ((score-adt 'verhoog! ) 5))
(define id (random 1 4))
(define kleur '())
(define (zoek-kleur)
  (cond ((= id 1) (set! kleur "yellow"))
        ((= id 2) (set! kleur "white"))
        ((= id 3) (set! kleur "red"))))

(define (activeer!)
  (zoek-kleur)
  ((teken-adt 'maak-power-up!))
  (set! staat 'actief)
  ;(set! random-nummer (+ random-nummer (random 1 10000)))

  )
(define (deactiveer!)
  (set! staat 'inactief))
(define (zet-terug! vloot-adt kogels-adt)
  (cond
    ((eq? kleur "yellow")
     ((vloot-adt 'herstart!)))
    ((eq? kleur "red")
     ((kogels-adt 'volgende-kogel!) 'normaal))
    ((eq? kleur "white")
     (vloot-adt 'versnel!) 0.5))
  (reset!))


  (define (dispatch-power-up msg)
    (cond
      ((eq? msg 'x!) set-positie-x!)
      ((eq? msg 'y!) set-positie-y!)
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'delete!) delete!)
      ((eq? msg 'kleur) kleur)
      ((eq? msg 'levens) levens)
      ((eq? msg 'geraakt!) geraakt!)
      ((eq? msg 'activeer!) activeer!)
      ((eq? msg 'deactiveer!) deactiveer!)
      ((eq? msg 'staat) staat)
      ((eq? msg 'random-nummer) random-nummer)
      ((eq? msg 'timer) timer)
      ((eq? msg 'reset!) reset!)
      ((eq? msg 'zet-terug!) zet-terug!)
      ))

  dispatch-power-up)
