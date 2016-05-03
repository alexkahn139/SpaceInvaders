#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Zo kunnen andere files deze bestanden gebruiken.
(provide venster-hoogte
         venster-breedte
         px-venster-hoogte
         px-venster-breedte
         px-alien-hoogte
         px-alien-breedte
         px-alien-reeel
         px-schip-hoogte
         px-schip-breedte
         px-kogel-hoogte
         px-kogel-breedte
         px-start-hoogte
         px-start-breedte
         px-rotator-hoogte
         px-rotator-breedte
         px-how-to-hoogte
         px-how-to-breedte
         helft
         increment
         decrement
         binnen-grenzen?
         check-geraakt?
         centraliseer
         timer5s)

;Speelvenster
(define venster-breedte 1)
(define venster-hoogte 1)

;Speelvenster in aantal pixels
(define px-venster-hoogte 600)
(define px-venster-breedte 600)

;Grootte van een alien in pixels
(define px-alien-hoogte 30)
(define px-alien-breedte 30)
;Grootte van een alien op het venster
(define px-alien-reeel
  (/ px-alien-hoogte px-venster-hoogte))
;Grootte van het schip in pixels
(define px-schip-hoogte 20)
(define px-schip-breedte 30)
;Grootte van de kogel in pixels
(define px-kogel-hoogte 8)
(define px-kogel-breedte 4)
;Grootte van de start-knop in pixels
(define px-start-hoogte 120)
(define px-start-breedte 80)
;Grootte van de rotator in pixels
(define px-rotator-hoogte 50)
(define px-rotator-breedte 50)
;Grootte van de how-to in pixels
(define px-how-to-hoogte 300)
(define px-how-to-breedte 277)



;Functie om de helft te berekenen.
(define (helft object)
  (/ object 2))

;Functie om een positie aan een bepaalde snelheid te doen stijgen
(define (increment positie snelheid)
  (+ positie snelheid))
(define (decrement positie snelheid)
  (- positie snelheid))

;Functie om te zien of het object zich nog binnen de grenzen van het spel bevindt?
(define (binnen-grenzen? positie omvang)
  (and (< 0 (- positie (helft omvang)))
       (> px-venster-hoogte (- positie (helft omvang)))))

;Functie om te zien of er een botsing tussen 2 objecten gebeurt.
(define (check-geraakt? object1 object2)
  (and (> (object1 'levens) 0)
       (and (and (<= (- (object1 'x) (helft px-alien-reeel)) (object2 'x))
                 (>= (+ (object1 'x) (helft px-alien-reeel)) (object2 'x)))
            (and (>= (+ (object1 'y) px-alien-reeel) (object2 'y))
                 (<= (object1 'y) (object2 'y))))))


;Functie om objecten te centraliseren. Anders worden ze teveel naar rechts getekend.
(define (centraliseer positie type)
  (cond ((eq? type 'alien) (- positie (helft px-alien-hoogte)))
        ((eq? type 'kogel) (- positie (helft px-kogel-hoogte)))
        ((eq? type 'start) (- positie (helft px-start-hoogte)))
        ((eq? type 'schip) (- positie (helft px-schip-breedte)))
        ((eq? type 'rotator) (- positie (helft px-rotator-breedte)))
        ((eq? type 'how-to) (- positie (helft px-how-to-breedte)))
        ))
(define timer5s 5000)
