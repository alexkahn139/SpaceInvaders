#lang racket
(provide maak-vloot-adt)
(require "teken-adt.rkt")
(require "Abstracties.rkt")
(require "Alien.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;
;;Vloot ADT;;
;;;;;;;;;;;;;
;Het vloot-adt zal in een lijst alle aliens bijhouden. Ook zal dit ervoor zorgen dat bepaalde functies op alle aliens toegepast kunnen worden.

(define (maak-vloot-adt aantal teken-adt)
  ;De (+ aantal 1) is om te zorgen dat ze mooi gecentreerd staan
  ;(define afstand-tussen-2 0)
  ;Dit maakt een lege vloot aan in de vorm van een lijst.
  (define vloot '())
  (define richting-vloot 'rechts)
  (define horizon-snelheid 0.002)
  (define vert-snelheid 0.2)
  (define afstand-tussen-2 0)

  ;maak-aliens! maakt aliens aan, deze zullen dan een positie en dergelijke hebben, met maximaal 10 per rij. en worden in een lijst opgeslagen.
  ;het teken-adt wordt ook aangesproken om daar de nieuwe alien-tiles aan te maken
  (define (maak-aliens! aantal pos-y)
    (define (maak-rij-aliens! teller pos-x pos-y aantal-op-rij kleur)
      (if (< teller aantal-op-rij)
          (begin  (let* ((nieuwe-alien (maak-alien-adt pos-x pos-y kleur)))
                    (set! vloot (cons nieuwe-alien vloot))
                    ((teken-adt 'nieuwe-alien!) nieuwe-alien))
                  (maak-rij-aliens! (+ teller 1) (- pos-x afstand-tussen-2) pos-y aantal-op-rij kleur))
          'ok))
    (if (> aantal 10)
        (begin
          (set! afstand-tussen-2 (/ venster-breedte 10))
          (maak-rij-aliens! 0 (- venster-breedte afstand-tussen-2) pos-y 10 'groen)
          (maak-aliens! (- aantal 10) (+ pos-y 0.1)))
        (begin
          (set! afstand-tussen-2 (/ venster-breedte aantal))
          (maak-rij-aliens! 0 (- venster-breedte afstand-tussen-2) pos-y aantal 'geel))))
  ;Zorgt ervoor dat voor elke alien oproept om zichzelf te tekenen
  (define (teken! teken-adt)
    (for-each (lambda (alien-adt)
                ((alien-adt 'teken!) teken-adt))
              vloot))

  ;Zorgt ervoor dat men een functie kan uitvoeren op elke alien (nodig voor de collision detection).
  (define (loop-over-vloot functie)
    (map functie vloot))

  (define (verwijder-alle-aliens)
    (for-each (lambda (alien-adt)
                ((alien-adt 'delete!) teken-adt))
              vloot))

  (define (verwijder-vloot!)
    (set! vloot '()))

  ;Zorgt voor de beweging van de aliens
  ;Gaat na of de aliens met de rand van het scherm botsen en laat ze dan terug draaien.
  (define (beweeg! spel)
    (define (daal!)
      (for-each (lambda (alien-adt)
                  (let ((y (alien-adt 'y)))
                    ((alien-adt 'y!) (increment y vert-snelheid))))
        vloot))
    (for-each (lambda (alien-adt)
                (let ((y (alien-adt 'y))
                      (x (alien-adt 'x)))
                  (if (< y 1)
                      (begin
                        ;(delete-dode-aliens)
                        (cond
                          ((eq? richting-vloot 'links) (if  (< 0 x)
                                                            ((alien-adt 'x!) (decrement x horizon-snelheid))
                                                            (begin
                                                              (set! richting-vloot 'rechts)
                                                              (daal!)
                                                              ((alien-adt 'x!) (increment x horizon-snelheid)))
                                                            ))
                          ((eq? richting-vloot 'rechts) (if (> 1 x)
                                                            ((alien-adt 'x!) (increment x horizon-snelheid))
                                                            (begin
                                                              (set! richting-vloot 'links)
                                                              (daal!)
                                                              ((alien-adt 'x!) (increment x horizon-snelheid)))))))
                      ((spel 'restart!)))))
              vloot))

  (define (delete-dode-aliens)
    (define (delete-loop lijst reslijst)
      (if (not (null? lijst))
          (if (= ((car lijst) 'levens) 0)
              (delete-loop (cdr lijst) reslijst)
              (delete-loop (cdr lijst) (cons (car lijst) reslijst)))
          (set! vloot (reverse reslijst))))
    (delete-loop vloot '()))
  (maak-aliens! aantal 0)
  (define (versnel! factor) ;versnelling kan ook negatief zijn
        (set! horizon-snelheid (* horizon-snelheid factor)))
 ;Zo kan dezelfde functie gebruikt worden om zowel de witte power-up te starten als de gele te stoppen
(define (stop!)
  (set! horizon-snelheid 0))
(define (herstart!)
  (set! horizon-snelheid 0.0005))

  ;Dispatch er met dit object geïnterageerd kan worden.
  (define (dispatch msg)
    (cond ((eq? msg 'maak-aliens!) maak-aliens!)
          ((eq? msg 'teken!) teken!)
          ((eq? msg 'loop-over-vloot) loop-over-vloot)
          ((eq? msg 'beweeg!) beweeg!)
          ((eq? msg 'verwijder-vloot!) verwijder-vloot!)
          ((eq? msg 'vloot) vloot)
          ((eq? msg 'delete-dode-aliens) delete-dode-aliens)
          ((eq? msg 'versnel!) versnel!)
          ((eq? msg 'stop!) stop!)
          ((eq? msg 'herstart!) herstart!)

          ))
  dispatch)
