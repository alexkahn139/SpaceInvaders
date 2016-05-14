#lang racket


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;
;;Alien ADT;;
;;;;;;;;;;;;;
;Het Alien ADT zal gewoon alien's oproepen. Daarentegen zal het Vloot ADT ervoor zorgen dat er meerdere samen op het scherm verschijnen en deze in een latere fase zullen kunnen bewegen door de beweging door te geven.
(provide maak-alien-adt)
(require "teken-adt.rkt")
(require "Abstracties.rkt")

(define (maak-alien-adt positie-x positie-y kleur teken-adt)
  (define levens 0)
  (cond ((eq? kleur 'geel)
         (set! levens 1))
        ((eq? kleur 'groen)
         (set! levens 2))
        ((eq? kleur 'paars)
         (set! levens 3)))
  ;Hulpfuncties
  ;Om nieuwe posities mee te geven
  (define (set-positie-x! nieuwe-x)
    (set! positie-x nieuwe-x))
  (define (set-positie-y! nieuwe-y)
    (set! positie-y nieuwe-y))
  ;Om de objecten te tekenen of te verwijderen
  (define (teken! teken-adt)
    ((teken-adt 'teken-alien!) dispatch-alien))
  (define (delete! teken-adt)
    ((teken-adt 'delete-alien!) dispatch-alien))
  ;Loop van wat er moet gebeuren als een alien geraakt is.
  ;De kogel moet gedelete worden, zodat deze niet op het scherm blijft.
  ;De alien moet indien hij geen levens meer heeft ook gedeletet worden.
  (define (geraakt! kogel teken-adt vloot-adt score-adt kogels-adt huidige-tijd spel)
    (teken-adt 'delete-kogel!)
    ((score-adt 'verhoog!) 10)
    (cond ((eq? 'normaal (kogel 'type))
           (levens! (- levens 1)))
          ((eq? 'speciaal (kogel 'type))
           (levens! (- levens 2))))
    (if (>= 0 levens)
        (begin
         (delete! teken-adt)
         ((vloot-adt 'delete-dode-aliens) spel))
        ((vloot-adt 'delete-dode-aliens) spel)))
  (define (levens! getal)
    (set! levens getal))

  ;Dispatch er met dit object geïnterageerd kan worden.
  (define (dispatch-alien msg)
    (cond
      ((eq? msg 'x!) set-positie-x!)
      ((eq? msg 'y!) set-positie-y!)
      ((eq? msg 'x) positie-x)
      ((eq? msg 'y) positie-y)
      ((eq? msg 'teken!) teken!)
      ((eq? msg 'delete!) delete!)
      ((eq? msg 'geraakt!) geraakt!)
      ((eq? msg 'levens) levens)
      ((eq? msg 'levens!) levens!)
      ((eq? msg 'kleur) kleur)
      ))

  ;Dispatch oproepen dit is noodzakelijk voor object-georienteerd programmeren in Scheme
  dispatch-alien)
