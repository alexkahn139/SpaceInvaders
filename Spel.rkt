#lang racket
(provide maak-adt-spel)
(require "teken-adt.rkt")
(require "Abstracties.rkt")
(require "Alien.rkt")
(require "Vloot.rkt")
(require "Schip.rkt")
(require "Kogel.rkt")
(require "Kogels-adt.rkt")
(require "Menu.rkt")
(require "rotator.rkt")
(require "howto.rkt")
(require "Score.rkt")
(require "Power-Ups.rkt")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;   Alexandre Kahn    ;;;;;;;;;
;;;;;;;;;      500067         ;;;;;;;;;
;;;;;;;;;     1e BA CW        ;;;;;;;;;
;;;;;;;;; alexkahn@vub.ac.be  ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;
;; SPEL ADT ;;
;;;;;;;;;;;;;;
;Dit adt zal de spellus bevatten. Ook zal het ervoor zorgen dat de input van het toetsenbord doorgegeven wordt aan de nodige spelelementen. Verder zal de collision detection ook in dit ADT gebeuren.

(define (maak-adt-spel)
  ;Maakt alle nodige adt's aan.
  (define teken-adt (maak-adt-teken "Space Invaders" px-venster-hoogte px-venster-breedte))
  (define schip-adt (maak-schip-adt))
  (define vloot-adt (maak-vloot-adt teken-adt))
  (define kogels-adt (maak-kogels-adt))
  (define menu-adt (maak-menu-adt))
  (define rotator-adt (maak-rotator-adt))
  (define how-to-adt (maak-how-to-adt))
  (define score-adt (maak-score-adt))
  (define Power-up-adt (maak-power-up-adt (/ (random 1 10) 10) (/ (random 5 10) 10) teken-adt))
  (define level 1)
  (define begin-aliens 10)
  (define spel-tijd 0)

  (define (pauze!)
    ((menu-adt 'staat!) 'pauze)
    ((teken-adt 'verwijder-alles))
    ((teken-adt 'maak-rotator))
    ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze)
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze)
    ((rotator-adt 'teken!) teken-adt)
    ((menu-adt 'teken!) teken-adt))

  (define (restart!)
    (pauze!)
    ((teken-adt 'reset-vloot!))
    ((vloot-adt 'verwijder-vloot!))
    ((vloot-adt 'maak-aliens!) begin-aliens 0)
    (set! level 1)
    ;(pauze!)
    ;((teken-adt 'delete-menu!))
    )

  (define (play)
    ((teken-adt 'herteken-alles!))
    ((menu-adt 'staat!) 'play)
    ((teken-adt 'delete-menu!))
    ;((menu-adt 'delete!) teken-adt)
    ((teken-adt 'set-toets-functie!) toets-naar-spel-play)
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-play)
    )
  (define (howto)
    ((menu-adt 'delete!) teken-adt)
    ((menu-adt 'staat!) 'howto)
    ((teken-adt 'maak-how-to))
    ((teken-adt 'set-toets-functie!) toets-naar-spel-howto)
    ((teken-adt 'teken-how-to!) how-to-adt)
    ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-howto)
    )
  (define (nieuw-level!)
    (set! level (+ level 1))
    ((vloot-adt 'maak-aliens!) (+ begin-aliens (floor (/ (* level begin-aliens) 2))) 0)
    ;((vloot-adt 'maak-aliens!) begin-aliens 0)
    ((vloot-adt 'maak-alien-tiles!))
    ;((teken-adt 'herteken-alles!))
    ;((teken-adt  'herteken-alles!))
    )

  ;Momenteel geen tijd op een oplossing te vinden om het spel te herstarten. Waarschijnlijk gaat dit het beste door de start functie op te splitsen zodat enkel de deze opnieuwe opgeroepen kan worden.
  ;vloot-adt 'verwijder-vloot!))
  ;spellus
  ;Leest de toetsenbordinput uit en stuurt dit door naar het spel.
  ;Afhankelijk van de staat van het spel
  (define (toets-naar-spel-play toets)
    (cond
      ((eq? toets 'right)
       ((schip-adt 'beweeg!) 'rechts))
      ((eq? toets 'left)
       ((schip-adt 'beweeg!) 'links))
      ((eq? toets #\space)
       ((kogels-adt 'maak-kogel!) (schip-adt 'x) teken-adt))
      ((eq? toets #\p)
       (pauze!))))
  (define (toets-naar-spel-pauze toets); pas menu-tekenen na move
    (cond
      ((eq? toets #\return)
       (cond ((= 0 (rotator-adt 'staat))
              (play))
             ((= 1 (rotator-adt 'staat))
              (howto))
             ((= 2 (rotator-adt 'staat))
              (exit))))
      ((eq? toets 'up)
       ((rotator-adt 'up!) teken-adt))
      ((eq? toets 'down)
       ((rotator-adt 'down!) teken-adt))
      ))
  (define (toets-naar-spel-howto toets)
    (cond ((eq? toets #\return)
           (begin
             ((menu-adt 'staat!) 'pauze)
             ((teken-adt 'delete-how-to!))
             ((teken-adt 'maak-rotator))
             ((teken-adt 'verwijder-alles))
             ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze)
             ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze)))

          ))


  ; Functies voor het starten van het spel
  (define (start)
    ((teken-adt 'verwijder-alles))
    ((teken-adt 'maak-rotator)) ;Is de selector om in het menu te kiezen wat we willen openen
    (set! spel-tijd 0)
    ((vloot-adt 'maak-aliens!) begin-aliens 0)
    ((menu-adt 'teken!) teken-adt)
    ((rotator-adt 'teken!) teken-adt))

  ;;Schietlus, moet enkel opgeroepen worden als er geschoten is
  ;; Oproepen om het spel te laten lopen
  (define (spel-lus-functie-play delta-tijd)
    (set! spel-tijd (+ spel-tijd delta-tijd))
    ((schip-adt 'teken!) teken-adt)
    ((vloot-adt 'teken!) teken-adt)
    ((vloot-adt 'beweeg!) dispatch-spel)
    ;Indien de kogel afgeschoten is wordt de collision detection gedaan. Is performanter op deze manier, anders moet er elke beurt voor elke alien gecheckt worden.
    ((kogels-adt 'beweeg!) teken-adt vloot-adt score-adt Power-up-adt spel-tijd dispatch-spel)
    ((kogels-adt 'teken!) teken-adt)
    ((score-adt 'teken!) teken-adt)
    (if (eq? 'actief (Power-up-adt 'staat)) ;als het al actief is, moet het getekend worden, of na 5s van het scherm verdwijnen
        (if (< spel-tijd (+ timer5s (Power-up-adt 'random-nummer)))
            ((Power-up-adt 'teken!) teken-adt)
            (begin ((Power-up-adt 'delete!) teken-adt)
                   ((Power-up-adt 'reset!))))
        (if (> spel-tijd (Power-up-adt 'random-nummer)) ;als het op inactief staat moet het op de goede moment getekend worden
            ((Power-up-adt 'activeer!))
            'ok)
        )
    (if (and (eq? 'bezig (Power-up-adt 'staat))
             (> spel-tijd (+ timer5s (Power-up-adt 'timer))))
        ((Power-up-adt 'zet-terug!) vloot-adt kogels-adt)
        'ok); met de timer zien of het groter is
    )
  (define (spel-lus-functie-pauze delta-tijd)
    ; Input voor in het menu
    'ok
    )
  (define (spel-lus-functie-howto delta-tijd);Input voor een extra menu
    'ok
    )


  ;(nu worden de adt's zoals het moet in een lus hertekend)
  ((teken-adt 'set-spel-lus-functie!) spel-lus-functie-pauze)
  ((teken-adt 'set-toets-functie!) toets-naar-spel-pauze)

  ;; Dispatch functie
  (define (dispatch-spel msg)
    (cond
      ((eq? msg 'start) start)
      ((eq? msg 'pauze!) pauze!)
      ((eq? msg 'restart!) restart!)
      ((eq? msg 'nieuw-level!) nieuw-level!)
      ))
  dispatch-spel)

;Maakt het ADT van het spel, moet wel nog opgeroepen worden.
(define spel (maak-adt-spel))

;Entry point om alles te laden
((spel 'start))
;https://docs.racket-lang.org/reference/strings.html
;
